package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, Props, ReceiveTimeout}
import akka.event.Logging.{DebugLevel, ErrorLevel, InfoLevel, LogLevel, WarningLevel}
import akka.util.ByteString
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
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class BlockImporter(
    fetcher: ActorRef,
    ledger: Ledger,
    blockchain: Blockchain,
    syncConfig: SyncConfig,
    ommersPool: ActorRef,
    broadcaster: ActorRef,
    pendingTransactionsManager: ActorRef,
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

  private def handleTopMessages(state: ImporterState, currentBehavior: ImporterState => Receive): Receive = {
    case OnTop => context become currentBehavior(ImporterState.onTop(state))
    case NotOnTop => context become currentBehavior(ImporterState.notOnTop(state))
  }

  private def running(state: ImporterState): Receive = handleTopMessages(state, running) orElse {
    case ReceiveTimeout => pickBlocks()
    case BlockFetcher.PickedBlocks(blocks) => importBlocks(blocks, state)
    case MinedBlock(block) =>
      if (!state.importing && state.isOnTop) {
        importMinedBlock(block, state)
      } else {
        ommersPool ! AddOmmers(block.header)
      }
    case ImportNewBlock(block, peerId) if state.isOnTop && !state.importing => importNewBlock(block, peerId, state)
    case ImportDone(newBehavior) =>
      val newState = ImporterState.notImportingBlocks(state)
      val behavior: Behavior = newBehavior match {
        case Running => running
        case ResolvingMissingNode(blocksToRetry) => resolvingMissingNode(blocksToRetry, _)
      }
      context become behavior(newState)
  }

  private def resolvingMissingNode(blocksToRetry: List[Block], state: ImporterState): Receive = {
    case BlockFetcher.FetchedStateNode(nodeData) =>
      val node = nodeData.values.head
      blockchain.saveNode(kec256(node), node.toArray, Block.number(blocksToRetry.head))
      importBlocks(blocksToRetry, state)
  }

  private def start(): Unit = {
    fetcher ! BlockFetcher.Start(self, blockchain.getBestBlockNumber())
    context become running(ImporterState.initial)
  }

  private def pickBlocks(): Unit =
    fetcher ! BlockFetcher.PickBlocks(blocksBatchSize)

  private def importBlocks(blocks: List[Block], state: ImporterState): Unit =
    importWith(
      state,
      Future
        .successful(resolveBranch(blocks))
        .flatMap(tryImportBlocks(_))
        .map(value => {
          val (importedBlocks, errorOpt) = value
          log.debug("imported blocks {}", importedBlocks.map(Block.number).mkString(","))

          errorOpt match {
            case None =>
              pickBlocks()
              Running
            case Some(err) =>
              log.error("block import error {}", err)
              val notImportedBlocks = blocks.drop(importedBlocks.size)
              val invalidBlockNr = Block.number(notImportedBlocks.head)

              err match {
                case e: MissingNodeException =>
                  fetcher ! BlockFetcher.FetchStateNode(e.hash)
                  ResolvingMissingNode(notImportedBlocks)
                case _ =>
                  fetcher ! BlockFetcher.InvalidateBlocksFrom(invalidBlockNr, err.toString)
                  pickBlocks()
                  Running
              }
          }
        })
    )

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
            Future.successful((importedBlocks, Some(err)))
        }
        .recover {
          case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
            (importedBlocks, Some(missingNodeEx))
        }
    }

  private def importMinedBlock(block: Block, state: ImporterState): Unit =
    importBlock(block, new MinedBlockImportMessages(block), state, informFetcherOnFail = false)

  private def importNewBlock(block: Block, peerId: PeerId, state: ImporterState): Unit =
    importBlock(block, new NewBlockImportMessages(block, peerId), state, informFetcherOnFail = true)

  private def importBlock(
      block: Block,
      importMessages: ImportMessages,
      state: ImporterState,
      informFetcherOnFail: Boolean): Unit = {
    def doLog(entry: (LogLevel, String)): Unit = log.log(entry._1, entry._2)

    importWith(
      state, {
        doLog(importMessages.preImport())
        println(importMessages.preImport())
        ledger
          .importBlock(block)(context.dispatcher)
          .tap {
            case BlockImportedToTop(importedBlocksData) =>
              val (blocks, receipts) = importedBlocksData.map(data => (data.block, data.td)).unzip
              broadcastBlocks(blocks, receipts)
              updateTxAndOmmerPools(importedBlocksData.map(_.block), Seq.empty)
              doLog(importMessages.importedToTheTop())

            case BlockEnqueued =>
              doLog(importMessages.enqueued())
              ommersPool ! AddOmmers(block.header)

            case DuplicateBlock =>
              doLog(importMessages.duplicated())

            case UnknownParent =>
              // This is normal when receiving broadcast blocks
              doLog(importMessages.orphaned())

            case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
              updateTxAndOmmerPools(newBranch, oldBranch)
              broadcastBlocks(newBranch, totalDifficulties)
              doLog(importMessages.reorganisedChain(newBranch))

            case BlockImportFailed(error) =>
              doLog(importMessages.importFailed(error))
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
      }
    )
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

  private def importWith(state: ImporterState, importFuture: => Future[NewBehavior]): Unit = {
    val newState = ImporterState.importingBlocks(state)
    context become running(newState)
    importFuture.onComplete {
      case Failure(ex) => throw ex
      case Success(behavior) => self ! ImportDone(behavior)
    }
  }

  private def resolveBranch(blocks: List[Block]): List[Block] =
    ledger.resolveBranch(blocks.map(_.header)) match {
      case NewBetterBranch(oldBranch) =>
        val transactionsToAdd = oldBranch.flatMap(_.body.transactionList).toSet
        pendingTransactionsManager ! PendingTransactionsManager.AddTransactions(transactionsToAdd)

        // Add first block from branch as an ommer
        oldBranch.headOption.foreach { h =>
          ommersPool ! AddOmmers(h.header)
        }
        blocks
      case NoChainSwitch =>
        // Add first block from branch as an ommer
        blocks.headOption
          .map(_.header)
          .foreach(
            header => {
              ommersPool ! AddOmmers(header)
              fetcher ! BlockFetcher.InvalidateBlocksFrom(header.number, "no progress on chain", withBlacklist = false)
            }
          )
        Nil
      case UnknownBranch =>
        blocks.headOption
          .map(Block.number)
          .foreach(number => {
            fetcher ! BlockFetcher.InvalidateBlocksFrom(number, "unknown branch")
          })
        Nil
      case InvalidBranch =>
        blocks.headOption
          .map(Block.number)
          .foreach(number => {
            fetcher ! BlockFetcher.InvalidateBlocksFrom(number, "invalid branch")
          })
        Nil
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

  sealed trait ImporterMsg
  case object Start extends ImporterMsg
  case object OnTop extends ImporterMsg
  case object NotOnTop extends ImporterMsg
  case class MinedBlock(block: Block) extends ImporterMsg
  case class ImportNewBlock(block: Block, peerId: PeerId) extends ImporterMsg
  case class ImportDone(newBehavior: NewBehavior) extends ImporterMsg

  sealed trait NewBehavior
  case object Running extends NewBehavior
  case class ResolvingMissingNode(blocksToRetry: List[Block]) extends NewBehavior

  case class ImporterState(isOnTop: Boolean, importing: Boolean)

  object ImporterState {
    val initial: ImporterState = ImporterState(isOnTop = false, importing = false)

    def onTop(state: ImporterState): ImporterState = state.copy(isOnTop = true)

    def notOnTop(state: ImporterState): ImporterState = state.copy(isOnTop = false)

    def importingBlocks(state: ImporterState): ImporterState = state.copy(importing = true)

    def notImportingBlocks(state: ImporterState): ImporterState = state.copy(importing = false)
  }

  sealed abstract class ImportMessages(block: Block) {
    protected lazy val hash: ByteString = block.header.hash
    protected lazy val number: BigInt = Block.number(block)

    def preImport(): (LogLevel, String)
    def importedToTheTop(): (LogLevel, String)
    def enqueued(): (LogLevel, String)
    def duplicated(): (LogLevel, String)
    def orphaned(): (LogLevel, String)
    def reorganisedChain(newBranch: List[Block]): (LogLevel, String)
    def importFailed(error: String): (LogLevel, String)
    def missingStateNode(exception: MissingNodeException): (LogLevel, String)
  }

  class MinedBlockImportMessages(block: Block) extends ImportMessages(block) {
    override def preImport(): (LogLevel, String) = (DebugLevel, s"Importing new mined block (${block.idTag})")
    override def importedToTheTop(): (LogLevel, String) =
      (DebugLevel, s"Added new mined block $number to top of the chain")
    override def enqueued(): (LogLevel, String) = (DebugLevel, s"Mined block $number was added to the queue")
    override def duplicated(): (LogLevel, String) =
      (WarningLevel, "Mined block is a duplicate, this should never happen")
    override def orphaned(): (LogLevel, String) = (WarningLevel, "Mined block has no parent on the main chain")
    override def reorganisedChain(newBranch: List[Block]): (LogLevel, String) =
      (DebugLevel, s"Addition of new mined block $number resulting in chain reorganization")
    override def importFailed(error: String): (LogLevel, String) =
      (WarningLevel, s"Failed to execute mined block because of $error")
    override def missingStateNode(exception: MissingNodeException): (LogLevel, String) =
      (ErrorLevel, s"Ignoring mined block $exception")
  }

  class NewBlockImportMessages(block: Block, peerId: PeerId) extends ImportMessages(block) {
    override def preImport(): (LogLevel, String) = (DebugLevel, s"Handling NewBlock message for block (${block.idTag})")
    override def importedToTheTop(): (LogLevel, String) =
      (InfoLevel, s"Added new block $number to the top of the chain received from $peerId")
    override def enqueued(): (LogLevel, String) = (DebugLevel, s"Block $number ($hash) from $peerId added to queue")
    override def duplicated(): (LogLevel, String) =
      (DebugLevel, s"Ignoring duplicate block $number ($hash) from $peerId")
    override def orphaned(): (LogLevel, String) = (DebugLevel, s"Ignoring orphaned block $number ($hash) from $peerId")
    override def reorganisedChain(newBranch: List[Block]): (LogLevel, String) = {
      val lastHeader = newBranch.last.header
      (
        DebugLevel,
        s"Imported block $number ($hash) from $peerId, " +
          s"resulting in chain reorganisation: new branch of length ${newBranch.size} with head at block " +
          s"${lastHeader.number} (${hash2string(lastHeader.hash)})")
    }
    override def importFailed(error: String): (LogLevel, String) =
      (DebugLevel, s"Failed to import block ${block.idTag} from $peerId")
    override def missingStateNode(exception: MissingNodeException): (LogLevel, String) =
      (ErrorLevel, s"Ignoring broadcast block, reason: $exception")
  }

  def hash2string(hash: ByteString): String = Hex.toHexString(hash.toArray[Byte])
}
