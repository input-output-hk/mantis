package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, Props, ReceiveTimeout}
import cats.data.NonEmptyList
import cats.instances.future._
import cats.instances.list._
import cats.syntax.apply._
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlocks
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, RemoveOmmers}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddTransactions, RemoveTransactions}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import mouse.all._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class BlockImporter(
    fetcher: ActorRef,
    ledger: Ledger,
    blockchain: Blockchain,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    broadcaster: ActorRef,
    pendingTransactionsManager: ActorRef
) extends Actor
    with ActorLogging {
  import BlockImporter._

  implicit val ec: ExecutionContext = context.dispatcher

  context.setReceiveTimeout(syncConfig.syncRetryInterval)

  val blocksBatchSize = 50

  override def receive: Receive = idle

  override def postRestart(reason: Throwable): Unit = {
    super.postRestart(reason)
    start()
  }

  private def idle: Receive = {
    case Start => start()
  }

  private def handleTopMessages(state: ImporterState, currentBehavior: Behavior): Receive = {
    case OnTop => context become currentBehavior(ImporterState.onTop(state))
    case NotOnTop => context become currentBehavior(ImporterState.notOnTop(state))
  }

  private def running(state: ImporterState): Receive = handleTopMessages(state, running) orElse {
    case ReceiveTimeout => pickBlocks(state)
    case BlockFetcher.PickedBlocks(blocks) => importBlocks(blocks)(state)
    case MinedBlock(block) =>
      if (!state.importing && state.isOnTop) {
        importMinedBlock(block, state)
      } else {
        ommersPool ! AddOmmers(block.header)
      }
    case ImportNewBlock(block, peerId) if state.isOnTop && !state.importing => importNewBlock(block, peerId, state)
    case ImportDone(newBehavior) =>
      val newState = state |> ImporterState.notImportingBlocks |> ImporterState.branchResolved
      val behavior: Behavior = newBehavior match {
        case Running => running
        case ResolvingMissingNode(blocksToRetry) => resolvingMissingNode(blocksToRetry)
        case ResolvingBranch(from) => resolvingBranch(from)
      }
      context become behavior(newState)
    case PickBlocks => pickBlocks(state)
  }

  private def resolvingMissingNode(blocksToRetry: NonEmptyList[Block])(state: ImporterState): Receive = {
    case BlockFetcher.FetchedStateNode(nodeData) =>
      val node = nodeData.values.head
      blockchain.saveNode(kec256(node), node.toArray, Block.number(blocksToRetry.head))
      importBlocks(blocksToRetry)(state)
  }

  private def resolvingBranch(from: BigInt)(state: ImporterState): Receive =
    running(ImporterState.resolvingBranch(from, state))

  private def start(): Unit = {
    val currentBest = getStartingBlockNumber()
    log.debug(
      "Starting Regular Sync, current best block with hash is {}, best by blockchain is {}",
      currentBest,
      blockchain.getBestBlockNumber())
    fetcher ! BlockFetcher.Start(self, currentBest)
    context become running(ImporterState.initial)
  }

  private def pickBlocks(state: ImporterState): Unit = {
    val currentBest = getStartingBlockNumber()
    log.debug(
      "Picking blocks, current best block with hash is {}, best by blockchain is {}",
      currentBest,
      blockchain.getBestBlockNumber())

    val msg = state.resolvingBranchFrom.fold[BlockFetcher.FetchMsg](
      BlockFetcher.PickBlocks(blocksBatchSize)
    )(
      from => BlockFetcher.StrictPickBlocks(from, currentBest)
    )

    fetcher ! msg
  }

  private def importBlocks(blocks: NonEmptyList[Block]): ImportFn =
    importWith({
      log.debug("Attempting to import blocks starting from {}", Block.number(blocks.head))
      Future
        .successful(resolveBranch(blocks))
        .flatMap({
          case Right(blocksToImport) => handleBlocksImport(blocksToImport)
          case Left(resolvingFrom) => Future.successful(ResolvingBranch(resolvingFrom))
        })
    })

  private def handleBlocksImport(blocks: List[Block]): Future[NewBehavior] =
    tryImportBlocks(blocks)
      .map(value => {
        val (importedBlocks, errorOpt) = value
        log.debug("imported blocks {}", importedBlocks.map(Block.number).mkString(","))

        errorOpt match {
          case None =>
            self ! PickBlocks
            Running
          case Some(err) =>
            log.error(
              "block import error {}, current best block is {} and best block with hash is {}",
              err,
              blockchain.getBestBlockNumber(),
              getStartingBlockNumber())
            val notImportedBlocks = blocks.drop(importedBlocks.size)

            err match {
              case e: MissingNodeException =>
                fetcher ! BlockFetcher.FetchStateNode(e.hash)
                ResolvingMissingNode(NonEmptyList(notImportedBlocks.head, notImportedBlocks.tail))
              case _ =>
                val invalidBlockNr = Block.number(notImportedBlocks.head)
                fetcher ! BlockFetcher.InvalidateBlocksFrom(invalidBlockNr, err.toString)
                self ! PickBlocks
                Running
            }
        }
      })

  private def tryImportBlocks(blocks: List[Block], importedBlocks: List[Block] = Nil)(
      implicit ec: ExecutionContext): Future[(List[Block], Option[Any])] =
    if (blocks.isEmpty) {
      Future.successful((importedBlocks, None))
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
            log.debug(s"block ${Block.number(blocks.head)} import failed")
            Future.successful((importedBlocks, Some(err)))
        }
        .recover {
          case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
            (importedBlocks, Some(missingNodeEx))
        }
    }

  private def importMinedBlock(block: Block, state: ImporterState): Unit =
    importBlock(block, new MinedBlockImportMessages(block), informFetcherOnFail = false)(state)

  private def importNewBlock(block: Block, peerId: PeerId, state: ImporterState): Unit =
    importBlock(block, new NewBlockImportMessages(block, peerId), informFetcherOnFail = true)(state)

  private def importBlock(block: Block, importMessages: ImportMessages, informFetcherOnFail: Boolean): ImportFn = {
    def doLog(entry: ImportMessages.LogEntry): Unit = log.log(entry._1, entry._2)

    importWith({
      doLog(importMessages.preImport())
      ledger
        .importBlock(block)(context.dispatcher)
        .tap(importMessages.messageForImportResult _ andThen doLog)
        .tap {
          case BlockImportedToTop(importedBlocksData) =>
            val (blocks, receipts) = importedBlocksData.map(data => (data.block, data.td)).unzip
            broadcastBlocks(blocks, receipts)
            updateTxAndOmmerPools(importedBlocksData.map(_.block), Seq.empty)

          case BlockEnqueued =>
            ommersPool ! AddOmmers(block.header)

          case DuplicateBlock => ()

          case UnknownParent => () // This is normal when receiving broadcast blocks

          case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
            updateTxAndOmmerPools(newBranch, oldBranch)
            broadcastBlocks(newBranch, totalDifficulties)

          case BlockImportFailed(error) =>
            if (informFetcherOnFail) {
              fetcher ! BlockFetcher.BlockImportFailed(Block.number(block), error)
            }
        }
        .map(_ => Running)
        .recover {
          case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
            // state node re-download will be handled when downloading headers
            doLog(importMessages.missingStateNode(missingNodeEx))
            Running
        }
    })
  }

  private def broadcastBlocks(blocks: List[Block], totalDifficulties: List[BigInt]): Unit = {
    val newBlocks = (blocks, totalDifficulties).mapN(NewBlock.apply)
    broadcastNewBlocks(newBlocks)
  }

  private def broadcastNewBlocks(blocks: List[NewBlock]): Unit = broadcaster ! BroadcastBlocks(blocks)

  private def updateTxAndOmmerPools(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.headOption.foreach(block => ommersPool ! AddOmmers(block.header))
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddTransactions(block.body.transactionList.toSet))

    blocksAdded.foreach(block => {
      ommersPool ! RemoveOmmers(block.header :: block.body.uncleNodesList.toList)
      pendingTransactionsManager ! RemoveTransactions(block.body.transactionList)
    })
  }

  private def importWith(importFuture: => Future[NewBehavior])(state: ImporterState): Unit = {
    val newState = ImporterState.importingBlocks(state)
    context become running(newState)
    importFuture.onComplete {
      case Failure(ex) => throw ex
      case Success(behavior) => self ! ImportDone(behavior)
    }
  }

  // Either block from which we try resolve branch or list of blocks to be imported
  private def resolveBranch(blocks: NonEmptyList[Block]): Either[BigInt, List[Block]] =
    ledger.resolveBranch(blocks.map(_.header).toList) match {
      case NewBetterBranch(oldBranch) =>
        val transactionsToAdd = oldBranch.flatMap(_.body.transactionList).toSet
        pendingTransactionsManager ! PendingTransactionsManager.AddTransactions(transactionsToAdd)

        // Add first block from branch as an ommer
        oldBranch.headOption.map(_.header).foreach(ommersPool ! AddOmmers(_))
        Right(blocks.toList)
      case NoChainSwitch =>
        // Add first block from branch as an ommer
        ommersPool ! AddOmmers(blocks.head.header)
        Right(Nil)
      case UnknownBranch =>
        val currentBlock = Block.number(blocks.head).min(getStartingBlockNumber())
        val goingBackTo = currentBlock - syncConfig.branchResolutionRequestSize
        val msg = s"Unknown branch, going back to block nr $goingBackTo in order to resolve branches"

        log.debug(msg)
        fetcher ! BlockFetcher.InvalidateBlocksFrom(goingBackTo, msg, shouldBlacklist = false)
        Left(goingBackTo)
      case InvalidBranch =>
        val goingBackTo = Block.number(blocks.head)
        val msg = s"invalid branch, going back to $goingBackTo"

        log.debug(msg)
        fetcher ! BlockFetcher.InvalidateBlocksFrom(goingBackTo, msg)
        Right(Nil)
    }

  @tailrec
  private def getStartingBlockNumber(current: BigInt = blockchain.getBestBlockNumber()): BigInt =
    if (blockchain.getBlockHeaderByNumber(current).isDefined) {
      current
    } else {
      getStartingBlockNumber(current - 1)
    }
}

object BlockImporter {

  def props(
      fetcher: ActorRef,
      ledger: Ledger,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      ommersPool: ActorRef,
      broadcaster: ActorRef,
      pendingTransactionsManager: ActorRef): Props =
    Props(
      new BlockImporter(fetcher, ledger, blockchain, syncConfig, ommersPool, broadcaster, pendingTransactionsManager))

  type Behavior = ImporterState => Receive
  type ImportFn = ImporterState => Unit

  sealed trait ImporterMsg
  case object Start extends ImporterMsg
  case object OnTop extends ImporterMsg
  case object NotOnTop extends ImporterMsg
  case class MinedBlock(block: Block) extends ImporterMsg
  case class ImportNewBlock(block: Block, peerId: PeerId) extends ImporterMsg
  case class ImportDone(newBehavior: NewBehavior) extends ImporterMsg
  case object PickBlocks extends ImporterMsg

  sealed trait NewBehavior
  case object Running extends NewBehavior
  case class ResolvingMissingNode(blocksToRetry: NonEmptyList[Block]) extends NewBehavior
  case class ResolvingBranch(from: BigInt) extends NewBehavior

  case class ImporterState(isOnTop: Boolean, importing: Boolean, resolvingBranchFrom: Option[BigInt])

  object ImporterState {
    val initial: ImporterState = ImporterState(isOnTop = false, importing = false, resolvingBranchFrom = None)

    def onTop(state: ImporterState): ImporterState = state.copy(isOnTop = true)

    def notOnTop(state: ImporterState): ImporterState = state.copy(isOnTop = false)

    def importingBlocks(state: ImporterState): ImporterState = state.copy(importing = true)

    def notImportingBlocks(state: ImporterState): ImporterState = state.copy(importing = false)

    def resolvingBranch(from: BigInt, state: ImporterState): ImporterState =
      state.copy(resolvingBranchFrom = Some(from))

    def branchResolved(state: ImporterState): ImporterState = state.copy(resolvingBranchFrom = None)

    def isResolvingBranch(state: ImporterState): Boolean = state.resolvingBranchFrom.isDefined
  }
}
