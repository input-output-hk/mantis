package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, NotInfluenceReceiveTimeout, Props, ReceiveTimeout}
import cats.data.NonEmptyList
import cats.implicits._
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlocks
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.ommers.OmmersPool.AddOmmers
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddUncheckedTransactions, RemoveTransactions}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
import monix.execution.Scheduler
import akka.actor.typed.{ActorRef => TypedActorRef}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHash, BlockHeaders, NewBlockHashes}
import io.iohk.ethereum.network.p2p.messages.{Codes, CommonMessages, PV64}
import akka.actor.typed.scaladsl.adapter._
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.UpdateKnownTop

import scala.concurrent.duration._

// scalastyle:off cyclomatic.complexity
class BlockImporter(
    fetcher: TypedActorRef[BlockFetcher.FetchCommand],
    peerEventBus: ActorRef,
    ledger: Ledger,
    blockchain: Blockchain,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    broadcaster: ActorRef,
    pendingTransactionsManager: ActorRef,
    supervisor: ActorRef
) extends Actor
    with ActorLogging {
  import BlockImporter._

  implicit val ec: Scheduler = Scheduler(context.dispatcher)

  context.setReceiveTimeout(syncConfig.syncRetryInterval)

  override def receive: Receive = idle

  override def postRestart(reason: Throwable): Unit = {
    super.postRestart(reason)
    start()
    peerEventBus ! Subscribe(
      MessageClassifier(
        Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
        PeerSelector.AllPeers
      )
    )
  }

  override def postStop(): Unit = {
    super.postStop()
    peerEventBus ! Unsubscribe()
  }

  private def idle: Receive = { case Start =>
    start()
    peerEventBus ! Subscribe(
      MessageClassifier(
        Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
        PeerSelector.AllPeers
      )
    )
  }

  private def running(state: ImporterState): Receive = {
    case ReceiveTimeout => self ! PickBlocks

    case PrintStatus => log.info("Best block: {}", blockchain.getBestBlockNumber())

    case BlockFetcher.PickedBlocks(blocks) =>
      SignedTransaction.retrieveSendersInBackGround(blocks.toList.map(_.body))
      importBlocks(blocks, DefaultBlockImport)(state)

    case MinedBlock(block) if !state.importing =>
      importBlock(
        block,
        new MinedBlockImportMessages(block),
        MinedBlockImport,
        internally = true
      )(state)

    //We don't want to lose a checkpoint
    case nc @ NewCheckpoint(_) if state.importing =>
      context.system.scheduler.scheduleOnce(1.second, self, nc)

    case NewCheckpoint(block) if !state.importing =>
      importBlock(
        block,
        new CheckpointBlockImportMessages(block),
        CheckpointBlockImport,
        internally = true
      )(state)

    case MessageFromPeer(BlockHeaders(headers), _) =>
      headers.lastOption.map { bh =>
        log.debug(s"Candidate for new top at block ${bh.number}, current known top ${state.knownTop}")
        val newState = state.withPossibleNewTopAt(bh.number)
        supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
        context become running(newState)
      }

    case MessageFromPeer(NewBlockHashes(hashes), _) =>
      val newState = state.validateNewBlockHashes(hashes) match {
        case Left(_) => state
        case Right(validHashes) => validHashes.lastOption.fold(state)(h => state.withPossibleNewTopAt(h.number))
      }
      log.debug("Received NewBlockHashes numbers {}", hashes.map(_.number).mkString(", "))
      supervisor ! ProgressProtocol.GotNewBlock(newState.knownTop)
      context become running(newState)

      //TODO: why doesn't work with a flag fitsOnTop
    case MessageFromPeer(CommonMessages.NewBlock(block, _), peerId) if !state.importing && state.fitsOnTop(block.number) =>
      supervisor ! ProgressProtocol.GotNewBlock(block.number)
      fetcher ! UpdateKnownTop(block.number)
      importBlock(
        block,
        new NewBlockImportMessages(block, peerId),
        NewBlockImport,
        internally = false
      )(state)

    case MessageFromPeer(PV64.NewBlock(block, _), peerId) if !state.importing && state.fitsOnTop(block.number) =>
      supervisor ! ProgressProtocol.GotNewBlock(block.number)
      fetcher ! UpdateKnownTop(block.number)
      importBlock(
        block,
        new NewBlockImportMessages(block, peerId),
        NewBlockImport,
        internally = false
      )(state)

    case ImportDone(newBehavior, importType) =>
      val newState = state.notImportingBlocks().branchResolved()
      val behavior: Behavior = getBehavior(newBehavior, importType)
      if (newBehavior == Running) {
        self ! PickBlocks
      }
      context become behavior(newState)

    case PickBlocks if !state.importing => pickBlocks(state)
  }

  private def resolvingMissingNode(blocksToRetry: NonEmptyList[Block], blockImportType: BlockImportType)(
      state: ImporterState
  ): Receive = { case BlockFetcher.FetchedStateNode(nodeData) =>
    val node = nodeData.values.head
    blockchain.saveNode(kec256(node), node.toArray, blocksToRetry.head.number)
    importBlocks(blocksToRetry, blockImportType)(state)
  }

  private def resolvingBranch(from: BigInt)(state: ImporterState): Receive =
    running(state.resolvingBranch(from))

  private def start(): Unit = {
    log.debug("Starting Regular Sync, current best block is {}", startingBlockNumber)
    fetcher ! BlockFetcher.Start(startingBlockNumber)
    supervisor ! ProgressProtocol.StartingFrom(startingBlockNumber)
    context become running(ImporterState.initial(startingBlockNumber, startingBlockNumber))
  }

  private def pickBlocks(state: ImporterState): Unit = {
    val msg =
      state.resolvingBranchFrom.fold[BlockFetcher.FetchCommand](BlockFetcher.PickBlocks(syncConfig.blocksBatchSize, self))(from =>
        BlockFetcher.StrictPickBlocks(from, startingBlockNumber, self)
      )

    fetcher ! msg
  }

  private def importBlocks(blocks: NonEmptyList[Block], blockImportType: BlockImportType)(state: ImporterState): Unit = importWith(
    {
      Task(
        log.debug(
          "Attempting to import blocks starting from {} and ending with {}",
          blocks.head.number,
          blocks.last.number
        )
      )
        .flatMap(_ => Task.now(resolveBranch(blocks)))
        .flatMap {
          case Right(blocksToImport) => handleBlocksImport(blocksToImport)(state)
          case Left(resolvingFrom) => Task.now(ResolvingBranch(resolvingFrom))
        }
    },
    blockImportType
  )(state)

  private def handleBlocksImport(blocks: List[Block])(state:ImporterState): Task[NewBehavior] =
    tryImportBlocks(blocks)
      .map { value =>
        val (importedBlocks, errorOpt) = value
        importedBlocks.size match {
          case 0 => log.debug("Imported no blocks")
          case 1 => {
            log.debug("Imported block {}", importedBlocks.head.number)
            fetcher ! BlockFetcher.LastImportedBlock(importedBlocks.head.number)
            context become running(state.withLastImportedBlock(importedBlocks.head.number))
          }
          case _ => {
            log.debug("Imported blocks {} - {}", importedBlocks.head.number, importedBlocks.last.number)
            fetcher ! BlockFetcher.LastImportedBlock(importedBlocks.last.number)
            context become running(state.withLastImportedBlock(importedBlocks.last.number))
          }
        }

        errorOpt match {
          case None => Running
          case Some(err) =>
            log.error("Block import error {}", err)
            val notImportedBlocks = blocks.drop(importedBlocks.size)

            err match {
              case e: MissingNodeException =>
                fetcher ! BlockFetcher.FetchStateNode(e.hash, self)
                ResolvingMissingNode(NonEmptyList(notImportedBlocks.head, notImportedBlocks.tail))
              case _ =>
                val invalidBlockNr = notImportedBlocks.head.number
                fetcher ! BlockFetcher.InvalidateBlocksFrom(invalidBlockNr, err.toString)
                Running
            }
        }
      }

  private def tryImportBlocks(
      blocks: List[Block],
      importedBlocks: List[Block] = Nil
  ): Task[(List[Block], Option[Any])] =
    if (blocks.isEmpty) {
      importedBlocks.headOption match {
        case Some(block) =>
          supervisor ! ProgressProtocol.ImportedBlock(block.number, internally = false)
        case None => ()
      }

      Task.now((importedBlocks, None))
    } else {
      val restOfBlocks = blocks.tail
      ledger
        .importBlock(blocks.head)
        .flatMap {
          case BlockImportedToTop(_) =>
            tryImportBlocks(restOfBlocks, blocks.head :: importedBlocks)

          case ChainReorganised(_, newBranch, _) =>
            tryImportBlocks(restOfBlocks, newBranch.reverse ::: importedBlocks)

          case DuplicateBlock | BlockEnqueued =>
            tryImportBlocks(restOfBlocks, importedBlocks)

          case err @ (UnknownParent | BlockImportFailed(_)) =>
            log.error("Block {} import failed", blocks.head.number)
            Task.now((importedBlocks, Some(err)))
        }
        .onErrorHandle {
          case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
            (importedBlocks, Some(missingNodeEx))
        }
    }

  private def importBlock(
      block: Block,
      importMessages: ImportMessages,
      blockImportType: BlockImportType,
      internally: Boolean
  )(state: ImporterState): Unit = {
    def doLog(entry: ImportMessages.LogEntry): Unit = log.log(entry._1, entry._2)
    importWith(
      {
        Task(doLog(importMessages.preImport()))
          .flatMap(_ => ledger.importBlock(block))
          .tap(importMessages.messageForImportResult _ andThen doLog)
          .tap {
            case BlockImportedToTop(importedBlocksData) =>
              val (blocks, weights) = importedBlocksData.map(data => (data.block, data.weight)).unzip
              broadcastBlocks(blocks, weights)
              updateTxPool(importedBlocksData.map(_.block), Seq.empty)
              supervisor ! ProgressProtocol.ImportedBlock(block.number, internally)
              fetcher ! BlockFetcher.LastImportedBlock(block.number)
              context become running(state.withLastImportedBlock(block.number))
            case BlockEnqueued => ()
            case DuplicateBlock => ()
            case UnknownParent => () // This is normal when receiving broadcast blocks
            case ChainReorganised(oldBranch, newBranch, weights) =>
              updateTxPool(newBranch, oldBranch)
              broadcastBlocks(newBranch, weights)
              newBranch.lastOption match {
                case Some(newBlock) =>
                  supervisor ! ProgressProtocol.ImportedBlock(newBlock.number, internally)
                  fetcher ! BlockFetcher.LastImportedBlock(newBlock.number)
                  context become running(state.withLastImportedBlock(newBlock.number))
                case None => ()
              }
              //TODO: return flag "informFetcherOnFail"?
            case BlockImportFailed(error) =>
                fetcher ! BlockFetcher.BlockImportFailed(block.number, error)
          }
          .map(_ => Running)
          .recover {
            case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
              // state node re-download will be handled when downloading headers
              doLog(importMessages.missingStateNode(missingNodeEx))
              Running
          }
      },
      blockImportType
    )(state)
  }

  private def broadcastBlocks(blocks: List[Block], weights: List[ChainWeight]): Unit = {
    val newBlocks = (blocks, weights).mapN(BlockToBroadcast)
    broadcaster ! BroadcastBlocks(newBlocks)
  }

  private def updateTxPool(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddUncheckedTransactions(block.body.transactionList))
    blocksAdded.foreach { block =>
      pendingTransactionsManager ! RemoveTransactions(block.body.transactionList)
    }
  }

  private def importWith(importTask: Task[NewBehavior], blockImportType: BlockImportType)(
      state: ImporterState
  ): Unit = {
    context become running(state.importingBlocks())

    importTask
      .map(self ! ImportDone(_, blockImportType))
      .onErrorHandle(ex => log.error(ex, ex.getMessage))
      .timed
      .map { case (timeTaken, _) => blockImportType.recordMetric(timeTaken.length) }
      .runAsyncAndForget
  }

  // Either block from which we try resolve branch or list of blocks to be imported
  private def resolveBranch(blocks: NonEmptyList[Block]): Either[BigInt, List[Block]] =
    ledger.resolveBranch(blocks.map(_.header)) match {
      case NewBetterBranch(oldBranch) =>
        val transactionsToAdd = oldBranch.flatMap(_.body.transactionList)
        pendingTransactionsManager ! PendingTransactionsManager.AddUncheckedTransactions(transactionsToAdd)

        // Add first block from branch as an ommer
        oldBranch.headOption.map(_.header).foreach(ommersPool ! AddOmmers(_))
        Right(blocks.toList)
      case NoChainSwitch =>
        // Add first block from branch as an ommer
        ommersPool ! AddOmmers(blocks.head.header)
        Right(Nil)
      case UnknownBranch =>
        val currentBlock = blocks.head.number.min(startingBlockNumber)
        val goingBackTo = (currentBlock - syncConfig.branchResolutionRequestSize).max(0)
        val msg = s"Unknown branch, going back to block nr $goingBackTo in order to resolve branches"

        log.info(msg)
        fetcher ! BlockFetcher.InvalidateBlocksFrom(goingBackTo, msg, shouldBlacklist = false)
        Left(goingBackTo)
      case InvalidBranch =>
        val goingBackTo = blocks.head.number
        val msg = s"Invalid branch, going back to $goingBackTo"

        log.info(msg)
        fetcher ! BlockFetcher.InvalidateBlocksFrom(goingBackTo, msg)
        Right(Nil)
    }

  private def startingBlockNumber: BigInt = blockchain.getBestBlockNumber()

  private def getBehavior(newBehavior: NewBehavior, blockImportType: BlockImportType): Behavior = newBehavior match {
    case Running => running
    case ResolvingMissingNode(blocksToRetry) => resolvingMissingNode(blocksToRetry, blockImportType)
    case ResolvingBranch(from) => resolvingBranch(from)
  }
}

object BlockImporter {
  // scalastyle:off parameter.number
  def props(
      fetcher: TypedActorRef[BlockFetcher.FetchCommand],
      peerEventBus: ActorRef,
      ledger: Ledger,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      broadcaster: ActorRef,
      pendingTransactionsManager: ActorRef,
      supervisor: ActorRef
  ): Props =
    Props(
      new BlockImporter(
        fetcher,
        peerEventBus,
        ledger,
        blockchain,
        syncConfig,
        ommersPool,
        broadcaster,
        pendingTransactionsManager,
        supervisor
      )
    )

  type Behavior = ImporterState => Receive

  sealed trait ImporterMsg
  case object Start extends ImporterMsg
  case class MinedBlock(block: Block) extends ImporterMsg
  case class NewCheckpoint(block: Block) extends ImporterMsg
  case class ImportDone(newBehavior: NewBehavior, blockImportType: BlockImportType) extends ImporterMsg
  case object PickBlocks extends ImporterMsg
  case object PrintStatus extends ImporterMsg with NotInfluenceReceiveTimeout

  sealed trait NewBehavior
  case object Running extends NewBehavior
  case class ResolvingMissingNode(blocksToRetry: NonEmptyList[Block]) extends NewBehavior
  case class ResolvingBranch(from: BigInt) extends NewBehavior

  sealed trait BlockImportType {
    def recordMetric(nanos: Long): Unit
  }

  case object MinedBlockImport extends BlockImportType {
    override def recordMetric(nanos: Long): Unit = RegularSyncMetrics.recordMinedBlockPropagationTimer(nanos)
  }

  case object CheckpointBlockImport extends BlockImportType {
    override def recordMetric(nanos: Long): Unit = RegularSyncMetrics.recordImportCheckpointPropagationTimer(nanos)
  }

  case object NewBlockImport extends BlockImportType {
    override def recordMetric(nanos: Long): Unit = RegularSyncMetrics.recordImportNewBlockPropagationTimer(nanos)
  }

  case object DefaultBlockImport extends BlockImportType {
    override def recordMetric(nanos: Long): Unit = RegularSyncMetrics.recordDefaultBlockPropagationTimer(nanos)
  }

  case class ImporterState(
      knownTop: BigInt,
      lastImportedBlock: BigInt,
      importing: Boolean,
      resolvingBranchFrom: Option[BigInt]
  ) {

    def importingBlocks(): ImporterState = copy(importing = true)

    def notImportingBlocks(): ImporterState = copy(importing = false)

    def resolvingBranch(from: BigInt): ImporterState = copy(resolvingBranchFrom = Some(from))

    def branchResolved(): ImporterState = copy(resolvingBranchFrom = None)

    def isResolvingBranch: Boolean = resolvingBranchFrom.isDefined

    def withLastImportedBlock(lastImportedBlock: BigInt): ImporterState = copy(lastImportedBlock = lastImportedBlock)

    def fitsOnTop(blockNumber: BigInt): Boolean = {
      println("1 " + blockNumber)
      println("2 " + lastImportedBlock)
      blockNumber == lastImportedBlock + 1
    }

    def withPossibleNewTopAt(top: BigInt): ImporterState =
      if (top > knownTop) {
        this.copy(knownTop = top)
      } else {
        this
      }

    def validateNewBlockHashes(hashes: Seq[BlockHash]): Either[String, Seq[BlockHash]] =
      hashes
        .asRight[String]
        .ensure("Hashes are empty")(_.nonEmpty)
        .ensure("Hashes should form a chain")(hashes =>
          hashes.zip(hashes.tail).forall { case (a, b) =>
            a.number + 1 == b.number
          }
        )
  }

  object ImporterState {
    def initial(knownTop: BigInt, lastImportedBlock: BigInt): ImporterState = ImporterState(
      knownTop = knownTop,
      lastImportedBlock = lastImportedBlock,
      importing = false,
      resolvingBranchFrom = None
    )
  }
}
