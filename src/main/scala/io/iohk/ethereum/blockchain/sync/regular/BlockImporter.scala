package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor
import akka.actor.Actor.Receive
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.NotInfluenceReceiveTimeout
import akka.actor.Props
import akka.actor.ReceiveTimeout

import cats.data.NonEmptyList
import cats.implicits._

import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlocks
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import io.iohk.ethereum.ommers.OmmersPool.AddOmmers
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.AddUncheckedTransactions
import io.iohk.ethereum.transactions.PendingTransactionsManager.RemoveTransactions
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._

class BlockImporter(
    fetcher: ActorRef,
    blockImport: BlockImport,
    blockchain: Blockchain,
    blockchainReader: BlockchainReader,
    stateStorage: StateStorage,
    branchResolution: BranchResolution,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    broadcaster: ActorRef,
    pendingTransactionsManager: ActorRef,
    supervisor: ActorRef,
    configBuilder: BlockchainConfigBuilder
) extends Actor
    with ActorLogging {
  import BlockImporter._
  import configBuilder._

  implicit val ec: Scheduler = Scheduler(context.dispatcher)

  context.setReceiveTimeout(syncConfig.syncRetryInterval)

  override def receive: Receive = idle

  override def postRestart(reason: Throwable): Unit = {
    super.postRestart(reason)
    start()
  }

  private def idle: Receive = { case Start =>
    start()
  }

  private def running(state: ImporterState): Receive = {
    case ReceiveTimeout => self ! PickBlocks

    case BlockFetcher.PickedBlocks(blocks) =>
      SignedTransaction.retrieveSendersInBackGround(blocks.toList.map(_.body))
      importBlocks(blocks, DefaultBlockImport)(state)

    case MinedBlock(block) if !state.importing =>
      importBlock(
        block,
        new MinedBlockImportMessages(block),
        MinedBlockImport,
        informFetcherOnFail = false,
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
        informFetcherOnFail = false,
        internally = true
      )(state)

    case ImportNewBlock(block, peerId) if !state.importing =>
      importBlock(
        block,
        new NewBlockImportMessages(block, peerId),
        NewBlockImport,
        informFetcherOnFail = true,
        internally = false
      )(state)

    case ImportDone(newBehavior, importType) =>
      val newState = state.notImportingBlocks().branchResolved()
      val behavior: Behavior = getBehavior(newBehavior, importType)
      if (newBehavior == Running) {
        self ! PickBlocks
      }
      context.become(behavior(newState))

    case PickBlocks if !state.importing => pickBlocks(state)
  }

  private def resolvingMissingNode(blocksToRetry: NonEmptyList[Block], blockImportType: BlockImportType)(
      state: ImporterState
  ): Receive = { case BlockFetcher.FetchedStateNode(nodeData) =>
    val node = nodeData.values.head
    stateStorage.saveNode(kec256(node), node.toArray, blocksToRetry.head.number)
    importBlocks(blocksToRetry, blockImportType)(state)
  }

  private def resolvingBranch(from: BigInt)(state: ImporterState): Receive =
    running(state.resolvingBranch(from))

  private def start(): Unit = {
    log.debug("Starting Regular Sync, current best block is {}", bestKnownBlockNumber)
    fetcher ! BlockFetcher.Start(self, bestKnownBlockNumber)
    supervisor ! ProgressProtocol.StartingFrom(bestKnownBlockNumber)
    context.become(running(ImporterState.initial))
  }

  private def pickBlocks(state: ImporterState): Unit = {
    val msg = state.resolvingBranchFrom.fold[BlockFetcher.FetchCommand](
      BlockFetcher.PickBlocks(syncConfig.blocksBatchSize, self)
    )(from => BlockFetcher.StrictPickBlocks(from, bestKnownBlockNumber, self))

    fetcher ! msg
  }

  private def importBlocks(blocks: NonEmptyList[Block], blockImportType: BlockImportType): ImportFn = importWith(
    Task
      .now {
        log.debug(
          "Attempting to import blocks starting from {} and ending with {}",
          blocks.head.number,
          blocks.last.number
        )
        resolveBranch(blocks)
      }
      .flatMap {
        case Right(blocksToImport) => handleBlocksImport(blocksToImport)
        case Left(resolvingFrom)   => Task.now(ResolvingBranch(resolvingFrom))
      },
    blockImportType
  )

  private def handleBlocksImport(blocks: List[Block]): Task[NewBehavior] =
    tryImportBlocks(blocks)
      .map { value =>
        val (importedBlocks, errorOpt) = value
        importedBlocks.size match {
          case 0 => log.debug("Imported no blocks")
          case 1 => log.debug("Imported block {}", importedBlocks.head.number)
          case _ => log.debug("Imported blocks {} - {}", importedBlocks.head.number, importedBlocks.last.number)
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
      importedBlocks.headOption.foreach(block =>
        supervisor ! ProgressProtocol.ImportedBlock(block.number, internally = false)
      )
      Task.now((importedBlocks, None))
    } else {
      val restOfBlocks = blocks.tail
      blockImport
        .importBlock(blocks.head)
        .flatMap {
          case BlockImportedToTop(_) =>
            tryImportBlocks(restOfBlocks, blocks.head :: importedBlocks)

          case ChainReorganised(_, newBranch, _) =>
            tryImportBlocks(restOfBlocks, newBranch.reverse ::: importedBlocks)

          case DuplicateBlock | BlockEnqueued =>
            tryImportBlocks(restOfBlocks, importedBlocks)

          case BlockImportFailedDueToMissingNode(missingNodeException) if syncConfig.redownloadMissingStateNodes =>
            Task.now((importedBlocks, Some(missingNodeException)))

          case BlockImportFailedDueToMissingNode(missingNodeException) =>
            Task.raiseError(missingNodeException)

          case err @ (UnknownParent | BlockImportFailed(_)) =>
            log.error(
              "Block {} import failed, with hash {} and parent hash {}",
              blocks.head.number,
              blocks.head.header.hashAsHexString,
              ByteStringUtils.hash2string(blocks.head.header.parentHash)
            )
            Task.now((importedBlocks, Some(err)))
        }
    }

  private def importBlock(
      block: Block,
      importMessages: ImportMessages,
      blockImportType: BlockImportType,
      informFetcherOnFail: Boolean,
      internally: Boolean
  ): ImportFn = {
    def doLog(entry: ImportMessages.LogEntry): Unit = log.log(entry._1, entry._2)
    importWith(
      Task(doLog(importMessages.preImport()))
        .flatMap(_ => blockImport.importBlock(block))
        .tap((importMessages.messageForImportResult _).andThen(doLog))
        .tap {
          case BlockImportedToTop(importedBlocksData) =>
            val (blocks, weights) = importedBlocksData.map(data => (data.block, data.weight)).unzip
            broadcastBlocks(blocks, weights)
            updateTxPool(importedBlocksData.map(_.block), Seq.empty)
            supervisor ! ProgressProtocol.ImportedBlock(block.number, internally)
          case ChainReorganised(oldBranch, newBranch, weights) =>
            updateTxPool(newBranch, oldBranch)
            broadcastBlocks(newBranch, weights)
            newBranch.lastOption.foreach(block => supervisor ! ProgressProtocol.ImportedBlock(block.number, internally))
          case BlockImportFailedDueToMissingNode(missingNodeException) if syncConfig.redownloadMissingStateNodes =>
            // state node re-download will be handled when downloading headers
            doLog(importMessages.missingStateNode(missingNodeException))
            Running
          case BlockImportFailedDueToMissingNode(missingNodeException) =>
            Task.raiseError(missingNodeException)
          case BlockImportFailed(error) if informFetcherOnFail =>
            fetcher ! BlockFetcher.BlockImportFailed(block.number, BlacklistReason.BlockImportError(error))
          case BlockEnqueued | DuplicateBlock | UnknownParent | BlockImportFailed(_) => ()
          case result                                                                => log.error("Unknown block import result {}", result)
        }
        .map(_ => Running),
      blockImportType
    )
  }

  private def broadcastBlocks(blocks: List[Block], weights: List[ChainWeight]): Unit = {
    val newBlocks = (blocks, weights).mapN(BlockToBroadcast)
    broadcaster ! BroadcastBlocks(newBlocks)
  }

  private def updateTxPool(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddUncheckedTransactions(block.body.transactionList))
    blocksAdded.foreach(block => pendingTransactionsManager ! RemoveTransactions(block.body.transactionList))
  }

  private def importWith(importTask: Task[NewBehavior], blockImportType: BlockImportType)(
      state: ImporterState
  ): Unit = {
    context.become(running(state.importingBlocks()))

    importTask
      .map(self ! ImportDone(_, blockImportType))
      .onErrorHandle(ex => log.error(ex, ex.getMessage))
      .timed
      .map { case (timeTaken, _) => blockImportType.recordMetric(timeTaken.length) }
      .runAsyncAndForget
  }

  // Either block from which we try resolve branch or list of blocks to be imported
  private def resolveBranch(blocks: NonEmptyList[Block]): Either[BigInt, List[Block]] =
    branchResolution.resolveBranch(blocks.map(_.header)) match {
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
        val currentBlock = blocks.head.number.min(bestKnownBlockNumber)
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

  private def bestKnownBlockNumber: BigInt = blockchainReader.getBestBlockNumber()

  private def getBehavior(newBehavior: NewBehavior, blockImportType: BlockImportType): Behavior = newBehavior match {
    case Running                             => running
    case ResolvingMissingNode(blocksToRetry) => resolvingMissingNode(blocksToRetry, blockImportType)
    case ResolvingBranch(from)               => resolvingBranch(from)
  }
}

object BlockImporter {
  // scalastyle:off parameter.number
  def props(
      fetcher: ActorRef,
      blockImport: BlockImport,
      blockchain: Blockchain,
      blockchainReader: BlockchainReader,
      stateStorage: StateStorage,
      branchResolution: BranchResolution,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      broadcaster: ActorRef,
      pendingTransactionsManager: ActorRef,
      supervisor: ActorRef,
      configBuilder: BlockchainConfigBuilder
  ): Props =
    Props(
      new BlockImporter(
        fetcher,
        blockImport,
        blockchain,
        blockchainReader,
        stateStorage,
        branchResolution,
        syncConfig,
        ommersPool,
        broadcaster,
        pendingTransactionsManager,
        supervisor,
        configBuilder
      )
    )

  type Behavior = ImporterState => Receive
  type ImportFn = ImporterState => Unit

  sealed trait ImporterMsg
  case object Start extends ImporterMsg
  case class MinedBlock(block: Block) extends ImporterMsg
  case class NewCheckpoint(block: Block) extends ImporterMsg
  case class ImportNewBlock(block: Block, peerId: PeerId) extends ImporterMsg
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
      importing: Boolean,
      resolvingBranchFrom: Option[BigInt]
  ) {
    def importingBlocks(): ImporterState = copy(importing = true)

    def notImportingBlocks(): ImporterState = copy(importing = false)

    def resolvingBranch(from: BigInt): ImporterState = copy(resolvingBranchFrom = Some(from))

    def branchResolved(): ImporterState = copy(resolvingBranchFrom = None)

    def isResolvingBranch: Boolean = resolvingBranchFrom.isDefined
  }

  object ImporterState {
    def initial: ImporterState = ImporterState(
      importing = false,
      resolvingBranchFrom = None
    )
  }
}
