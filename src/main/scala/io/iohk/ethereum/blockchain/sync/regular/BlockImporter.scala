package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging, ActorRef, Props, ReceiveTimeout}
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
import io.iohk.ethereum.transactions.PendingTransactionsManager.{AddTransactions, RemoveTransactions}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.FunctorOps._
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration._
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

  context.setReceiveTimeout(10.seconds)

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

  private def pickBlocks(): Unit = {
    fetcher ! BlockFetcher.PickBlocks(blocksBatchSize)
  }

  private def importBlocks(blocks: List[Block], state: ImporterState): Unit =
    importWith(
      state,
      tryImportBlocks(blocks).map { value =>
        {
          val (importedBlocks, errorOpt) = value
          log.debug("imported blocks {}", importedBlocks.map(Block.number).mkString(","))

          errorOpt match {
            case None =>
              pickBlocks()
              running
            case Some(err) =>
              log.error("block import error {}", err)
              val notImportedBlocks = blocks.drop(importedBlocks.size)
              val invalidBlockNr = Block.number(notImportedBlocks.head)

              err match {
                case e: MissingNodeException =>
                  fetcher ! BlockFetcher.FetchStateNode(e.hash)
                  resolvingMissingNode(notImportedBlocks, _)
                case _ =>
                  fetcher ! BlockFetcher.InvalidateBlocksFrom(invalidBlockNr, err.toString)
                  pickBlocks()
                  running
              }
          }
        }
      }
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
    importWith(
      state, {
        lazy val blockNumber = Block.number(block)
        ledger
          .importBlock(block)
          .tap {
            case BlockImportedToTop(importedBlocksData) =>
              val blockData = importedBlocksData.map(data => (data.block, data.td)).unzip
              log.debug(s"Added new mined block $blockNumber to top of the chain")
              broadcastBlocks(blockData._1, blockData._2)
              updateTxAndOmmerPools(importedBlocksData.map(_.block), Nil)

            case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
              log.debug(s"Added new mined block $blockNumber resulting in chain reorganization")
              broadcastBlocks(newBranch, totalDifficulties)
              updateTxAndOmmerPools(newBranch, oldBranch)

            case DuplicateBlock =>
              log.warning("Mined block is a duplicate, this should never happen")

            case BlockEnqueued =>
              log.debug(s"Mined block $blockNumber was added to the queue")
              ommersPool ! AddOmmers(block.header)

            case UnknownParent =>
              log.warning("Mined block has no parent on the main chain")

            case BlockImportFailed(err) =>
              log.warning(s"Failed to execute mined block because of $err")
          }
          .map(_ => running _)
          .recover {
            case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
              log.error("Ignoring mined block {}", missingNodeEx)
              running
          }
      }
    )

  private def importNewBlock(block: Block, peerId: PeerId, state: ImporterState): Unit =
    importWith(
      state, {
        log.debug(s"Handling NewBlock message for block (${block.idTag})")
        lazy val headerHash = hash2string(block.header.hash)
        lazy val newNumber = Block.number(block)
        ledger
          .importBlock(block)(context.dispatcher)
          .tap {
            case BlockImportedToTop(importedBlocksData) =>
              val (blocks, receipts) = importedBlocksData.map(data => (data.block, data.td)).unzip
              broadcastBlocks(blocks, receipts)
              updateTxAndOmmerPools(importedBlocksData.map(_.block), Seq.empty)
              log.info(s"Added new block $newNumber to the top of the chain received from $peerId")

            case BlockEnqueued =>
              log.debug(s"Block $newNumber ($headerHash) from $peerId added to queue")
              ommersPool ! AddOmmers(block.header)

            case DuplicateBlock =>
              log.debug(s"Ignoring duplicate block $newNumber ($headerHash) from $peerId")

            case UnknownParent =>
              // This is normal when receiving broadcast blocks
              log.debug(s"Ignoring orphaned block $newNumber ($headerHash) from $peerId")

            case ChainReorganised(oldBranch, newBranch, totalDifficulties) =>
              updateTxAndOmmerPools(newBranch, oldBranch)
              broadcastBlocks(newBranch, totalDifficulties)
              val header = newBranch.last.header
              log.debug(
                s"Imported block $newNumber ($headerHash) from $peerId, " +
                  s"resulting in chain reorganisation: new branch of length ${newBranch.size} with head at block " +
                  s"${header.number} (${hash2string(header.hash)})")

            case BlockImportFailed(error) =>
              fetcher ! BlockFetcher.BlockImportFailed(Block.number(block), error)
          }
          .map(_ => running _)
          .recover {
            case missingNodeEx: MissingNodeException if syncConfig.redownloadMissingStateNodes =>
              // state node re-download will be handled when downloading headers
              log.error("Ignoring broadcast block, reason: {}", missingNodeEx)
              running
          }
      }
    )

  private def broadcastBlocks(blocks: List[Block], totalDifficulties: List[BigInt]): Unit = {
    val newBlocks = (blocks, totalDifficulties).mapN(NewBlock.apply)
    broadcastNewBlocks(newBlocks)
  }

  private def broadcastNewBlocks(blocks: List[NewBlock]): Unit = {
    broadcaster ! BroadcastBlocks(blocks)
  }

  private def updateTxAndOmmerPools(blocksAdded: Seq[Block], blocksRemoved: Seq[Block]): Unit = {
    blocksRemoved.headOption.foreach(block => ommersPool ! AddOmmers(block.header))
    blocksRemoved.foreach(block => pendingTransactionsManager ! AddTransactions(block.body.transactionList.toSet))

    blocksAdded.foreach { block =>
      ommersPool ! RemoveOmmers(block.header :: block.body.uncleNodesList.toList)
      pendingTransactionsManager ! RemoveTransactions(block.body.transactionList)
    }
  }

  private def importWith(state: ImporterState, importFuture: => Future[Behavior]): Unit = {
    val newState = ImporterState.importingBlocks(state)
    context become running(newState)
    importFuture.onComplete {
      case Failure(ex) => throw ex
      case Success(behavior) => context become behavior(ImporterState.notImportingBlocks(newState))
    }
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

  case class ImporterState(isOnTop: Boolean, importing: Boolean)
  object ImporterState {
    val initial: ImporterState = ImporterState(isOnTop = false, importing = false)

    def onTop(state: ImporterState): ImporterState = state.copy(isOnTop = true)

    def notOnTop(state: ImporterState): ImporterState = state.copy(isOnTop = false)

    def importingBlocks(state: ImporterState): ImporterState = state.copy(importing = true)

    def notImportingBlocks(state: ImporterState): ImporterState = state.copy(importing = false)
  }

  def hash2string(hash: ByteString): String = Hex.toHexString(hash.toArray[Byte])
}
