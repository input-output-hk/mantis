package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorRef, NotInfluenceReceiveTimeout, Props, ReceiveTimeout}
import akka.util.ByteString
import cats.data.NonEmptyList
import cats.implicits._
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlocks
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.crypto.{ECDSASignature, kec256}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.ommers.OmmersPool.AddOmmers
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddUncheckedTransactions, RemoveTransactions}
import io.iohk.ethereum.utils.{ByteStringUtils, Logger}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

// scalastyle:off cyclomatic.complexity parameter.number
class BlockImporter(
    fetcher: ActorRef,
    ledger: Ledger,
    blockchain: Blockchain,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    broadcaster: ActorRef,
    pendingTransactionsManager: ActorRef,
    checkpointBlockGenerator: CheckpointBlockGenerator,
    supervisor: ActorRef
) extends Actor
    with Logger {
  import BlockImporter._

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

  private def handleTopMessages(state: ImporterState, currentBehavior: Behavior): Receive = {
    case OnTop => context become currentBehavior(state.onTop())
    case NotOnTop => context become currentBehavior(state.notOnTop())
  }

  private def running(state: ImporterState): Receive = handleTopMessages(state, running) orElse {
    case ReceiveTimeout => self ! PickBlocks

    case PrintStatus => log.info("Block: {}, is on top?: {}", blockchain.getBestBlockNumber(), state.isOnTop)

    case BlockFetcher.PickedBlocks(blocks) =>
      SignedTransaction.retrieveSendersInBackGround(blocks.toList.map(_.body))
      importBlocks(blocks)(state)

    //TODO ETCM-389: Handle mined, checkpoint and new blocks uniformly
    case MinedBlock(block) =>
      if (!state.importing) {
        importMinedBlock(block, state)
      }

    //TODO ETCM-389: Handle mined, checkpoint and new blocks uniformly
    case nc @ NewCheckpoint(parentHash, signatures) =>
      if (state.importing) {
        //We don't want to lose a checkpoint
        context.system.scheduler.scheduleOnce(1.second, self, nc)
      } else {
        ledger.getBlockByHash(parentHash) match {
          case Some(parent) =>
            val checkpointBlock = checkpointBlockGenerator.generate(parent, Checkpoint(signatures))
            importCheckpointBlock(checkpointBlock, state)

          case None =>
            log.error(s"Could not find parent (${ByteStringUtils.hash2string(parentHash)}) for new checkpoint block")
        }
      }

    case ImportNewBlock(block, peerId) if state.isOnTop && !state.importing => importNewBlock(block, peerId, state)

    case ImportDone(newBehavior) =>
      val newState = state.notImportingBlocks().branchResolved()
      val behavior: Behavior = getBehavior(newBehavior)

      if (newBehavior == Running) {
        self ! PickBlocks
      }

      context become behavior(newState)
    case PickBlocks if !state.importing => pickBlocks(state)
  }

  private def resolvingMissingNode(blocksToRetry: NonEmptyList[Block])(state: ImporterState): Receive = {
    case BlockFetcher.FetchedStateNode(nodeData) =>
      val node = nodeData.values.head
      blockchain.saveNode(kec256(node), node.toArray, blocksToRetry.head.number)
      importBlocks(blocksToRetry)(state)
  }

  private def resolvingBranch(from: BigInt)(state: ImporterState): Receive =
    running(state.resolvingBranch(from))

  private def start(): Unit = {
    log.debug("Starting Regular Sync, current best block is {}", startingBlockNumber)
    fetcher ! BlockFetcher.Start(self, startingBlockNumber)
    supervisor ! ProgressProtocol.StartingFrom(startingBlockNumber)
    context become running(ImporterState.initial)
  }

  private def pickBlocks(state: ImporterState): Unit = {
    val msg =
      state.resolvingBranchFrom.fold[BlockFetcher.FetchMsg](BlockFetcher.PickBlocks(syncConfig.blocksBatchSize))(from =>
        BlockFetcher.StrictPickBlocks(from, startingBlockNumber)
      )

    fetcher ! msg
  }

  private def importBlocks(blocks: NonEmptyList[Block]): ImportFn = importWith {
    Task(
      log.debug(
        "Attempting to import blocks starting from {} and ending with {}",
        blocks.head.number,
        blocks.last.number
      )
    )
      .flatMap(_ => Task.now(resolveBranch(blocks)))
      .flatMap {
        case Right(blocksToImport) => handleBlocksImport(blocksToImport)
        case Left(resolvingFrom) => Task.now(ResolvingBranch(resolvingFrom))
      }
  }

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
                fetcher ! BlockFetcher.FetchStateNode(e.hash)
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
        case Some(block) => supervisor ! ProgressProtocol.ImportedBlock(block.number, internally = false)
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

  private def importMinedBlock(block: Block, state: ImporterState): Unit =
    importBlock(
      block,
      new MinedBlockImportMessages(block),
      informFetcherOnFail = false,
      internally = true
    )(state)

  private def importCheckpointBlock(block: Block, state: ImporterState): Unit =
    importBlock(
      block,
      new CheckpointBlockImportMessages(block),
      informFetcherOnFail = false,
      internally = true
    )(state)

  private def importNewBlock(block: Block, peerId: PeerId, state: ImporterState): Unit =
    importBlock(
      block,
      new NewBlockImportMessages(block, peerId),
      informFetcherOnFail = true,
      internally = false
    )(state)

  private def importBlock(
      block: Block,
      importMessages: ImportMessages,
      informFetcherOnFail: Boolean,
      internally: Boolean
  ): ImportFn = {
    def doLog(entry: ImportMessages.LogEntry): Unit = log.info(entry._2) //log.log(entry._1, entry._2)

    importWith {
      Task(doLog(importMessages.preImport()))
        .flatMap(_ => ledger.importBlock(block))
        .tap(importMessages.messageForImportResult _ andThen doLog)
        .tap {
          case BlockImportedToTop(importedBlocksData) =>
            val (blocks, weights) = importedBlocksData.map(data => (data.block, data.weight)).unzip
            broadcastBlocks(blocks, weights)
            updateTxPool(importedBlocksData.map(_.block), Seq.empty)
            supervisor ! ProgressProtocol.ImportedBlock(block.number, internally)

          case BlockEnqueued => ()

          case DuplicateBlock => ()

          case UnknownParent => () // This is normal when receiving broadcast blocks

          case ChainReorganised(oldBranch, newBranch, weights) =>
            updateTxPool(newBranch, oldBranch)
            broadcastBlocks(newBranch, weights)
            newBranch.lastOption match {
              case Some(newBlock) => supervisor ! ProgressProtocol.ImportedBlock(newBlock.number, internally)
              case None => ()
            }

          case BlockImportFailed(error) =>
            if (informFetcherOnFail) {
              fetcher ! BlockFetcher.BlockImportFailed(block.number, error)
            }
        }
        .map(_ => Running)
        .recover {
          case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
            // state node re-download will be handled when downloading headers
            doLog(importMessages.missingStateNode(missingNodeEx))
            Running
        }
    }
  }

  private def broadcastBlocks(blocks: List[Block], weights: List[ChainWeight]): Unit = {
    val newBlocks = (blocks, weights).mapN(BlockToBroadcast(_, _))
    broadcaster ! BroadcastBlocks(newBlocks)
  }

  private def updateTxPool(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddUncheckedTransactions(block.body.transactionList))
    blocksAdded.foreach { block =>
      pendingTransactionsManager ! RemoveTransactions(block.body.transactionList)
    }
  }

  private def importWith(importTask: Task[NewBehavior])(state: ImporterState): Unit = {
    context become running(state.importingBlocks())
    importTask
      .map(self ! ImportDone(_))
      .onErrorHandle(ex => log.error(ex.getMessage, ex))
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
        val goingBackTo = currentBlock - syncConfig.branchResolutionRequestSize
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

  private def getBehavior(newBehavior: NewBehavior): Behavior = newBehavior match {
    case Running => running
    case ResolvingMissingNode(blocksToRetry) => resolvingMissingNode(blocksToRetry)
    case ResolvingBranch(from) => resolvingBranch(from)
  }
}

object BlockImporter {
  // scalastyle:off parameter.number
  def props(
      fetcher: ActorRef,
      ledger: Ledger,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      broadcaster: ActorRef,
      pendingTransactionsManager: ActorRef,
      checkpointBlockGenerator: CheckpointBlockGenerator,
      supervisor: ActorRef
  ): Props =
    Props(
      new BlockImporter(
        fetcher,
        ledger,
        blockchain,
        syncConfig,
        ommersPool,
        broadcaster,
        pendingTransactionsManager,
        checkpointBlockGenerator,
        supervisor
      )
    )

  type Behavior = ImporterState => Receive
  type ImportFn = ImporterState => Unit

  sealed trait ImporterMsg
  case object Start extends ImporterMsg
  case object OnTop extends ImporterMsg
  case object NotOnTop extends ImporterMsg
  case class MinedBlock(block: Block) extends ImporterMsg
  case class NewCheckpoint(parentHash: ByteString, signatures: Seq[ECDSASignature]) extends ImporterMsg
  case class ImportNewBlock(block: Block, peerId: PeerId) extends ImporterMsg
  case class ImportDone(newBehavior: NewBehavior) extends ImporterMsg
  case object PickBlocks extends ImporterMsg
  case object PrintStatus extends ImporterMsg with NotInfluenceReceiveTimeout

  sealed trait NewBehavior
  case object Running extends NewBehavior
  case class ResolvingMissingNode(blocksToRetry: NonEmptyList[Block]) extends NewBehavior
  case class ResolvingBranch(from: BigInt) extends NewBehavior

  case class ImporterState(
      isOnTop: Boolean,
      importing: Boolean,
      resolvingBranchFrom: Option[BigInt]
  ) {
    def onTop(): ImporterState = copy(isOnTop = true)

    def notOnTop(): ImporterState = copy(isOnTop = false)

    def importingBlocks(): ImporterState = copy(importing = true)

    def notImportingBlocks(): ImporterState = copy(importing = false)

    def resolvingBranch(from: BigInt): ImporterState = copy(resolvingBranchFrom = Some(from))

    def branchResolved(): ImporterState = copy(resolvingBranchFrom = None)

    def isResolvingBranch: Boolean = resolvingBranchFrom.isDefined
  }

  object ImporterState {
    def initial: ImporterState = ImporterState(
      isOnTop = false,
      importing = false,
      resolvingBranchFrom = None
    )
  }
}
