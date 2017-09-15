package io.iohk.ethereum.blockchain.sync

import akka.actor._
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockExecutionError, Ledger}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.ommers.OmmersPool.{AddOmmers, RemoveOmmers}
import io.iohk.ethereum.transactions.PendingTransactionsManager.AddTransactions
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.validators.Validators
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global

class RegularSync(
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val validators: Validators,
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val ommersPool: ActorRef,
    val pendingTransactionsManager: ActorRef,
    val ledger: Ledger,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler)
  extends Actor with ActorLogging with PeerListSupport with BlacklistSupport with SyncBlocksValidator with BlockBroadcast {

  import RegularSync._
  import syncConfig._

  private var headersQueue: Seq[BlockHeader] = Nil
  private var waitingForActor: Option[ActorRef] = None
  private var resolvingBranches: Boolean = false
  private var resumeRegularSyncTimeout: Option[Cancellable] = None

  scheduler.schedule(printStatusInterval, printStatusInterval, self, PrintStatus)

  peerEventBus ! Subscribe(MessageClassifier(Set(NewBlock.code), PeerSelector.AllPeers))

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  override def receive: Receive = idle

  def idle: Receive = handleCommonMessages orElse {
    case Start =>
      log.info("Starting block synchronization")
      appStateStorage.fastSyncDone()
      context become running
      askForHeaders()
  }

  private def resumeRegularSync(): Unit = {
    resumeRegularSyncTimeout.foreach(_.cancel)
    resumeRegularSyncTimeout = None
    askForHeaders()
  }

  /**
    * Handles broadcasted blocks, processing them and inserting them to the blockchain if necessary
    * FIXME: Current implementation is naive in that NewBlock msgs that can't be immediately processed are discarded
    *        and requested again to the peers
    */
  def handleBroadcastedBlockMessages: Receive = {
    case MessageFromPeer(NewBlock(newBlock, _), peerId) =>
      //we allow inclusion of new block only if we are not syncing / reorganising chain
      if (headersQueue.isEmpty && waitingForActor.isEmpty) {
        val currentBestBlock = appStateStorage.getBestBlockNumber()
        val maybeBlockParentTd = blockchain.getBlockHeaderByHash(newBlock.header.parentHash)
          .flatMap(b => blockchain.getTotalDifficultyByHash(b.hash))
        val maybeCurrentBlockTd = blockchain.getTotalDifficultyByNumber(currentBestBlock)

        (maybeBlockParentTd, maybeCurrentBlockTd) match {
          case (_, None) =>
            //Something went wrong, we don't have the total difficulty of the latest block
            //TODO: Investigate if we can recover from this error [EC-165]
            log.error(s"No total difficulty for the latest block with number $currentBestBlock")
            context stop self

          case (None, Some(_)) =>
            //The received block should be imported, however we don't have it's parent.
            //This could be caused by our client not being up-to-date or a fork in the blockchain
            //RegularSync is resumed which will lead to the needed reorganization
            log.debug("Resumed regular sync due to receiving NewBlock and not having it's parent")
            resumeRegularSync()

          case (Some(parentTd), Some(currentTd)) if currentTd >= parentTd + newBlock.header.difficulty =>
            //The received block is not imported as it's total difficulty is too low, do nothing
            log.debug("Not inserting NewBlock due to having too low total difficulty")

          case (Some(parentTd), Some(_)) =>
            val blockProcessResult = processBlock(newBlock, parentTd)
            blockProcessResult match {
              case Right(_) =>
                //Delete old blocks no longer needed
                if(newBlock.header.number < currentBestBlock) {
                  val oldBlocks = getOldBlocks((newBlock.header.number + 1) to currentBestBlock)
                  val oldBlocksTxs = oldBlocks.flatMap(_.body.transactionList)

                  pendingTransactionsManager ! AddTransactions(oldBlocksTxs.filterNot(newBlock.body.transactionList.contains))
                  oldBlocks.foreach{ b => blockchain.removeBlock(b.header.hash) }
                }
                log.debug(s"Added new block ${newBlock.header.number} received from $peerId")
              case Left(err) =>
                blacklist(peerId, blacklistDuration, err.toString)
            }
        }
      }
  }

  def handleResponseToRequest: Receive = {
    case ResponseReceived(peer: Peer, BlockHeaders(headers), timeTaken) =>
      log.info("Received {} block headers in {} ms", headers.size, timeTaken)
      waitingForActor = None
      if (resolvingBranches) handleBlockBranchResolution(peer, headers.reverse)
      else handleDownload(peer, headers)

    case ResponseReceived(peer, BlockBodies(blockBodies), timeTaken) =>
      log.info("Received {} block bodies in {} ms", blockBodies.size, timeTaken)
      waitingForActor = None
      handleBlockBodies(peer, blockBodies)

    case PeerRequestHandler.RequestFailed(peer, reason) if waitingForActor.contains(sender()) =>
      waitingForActor = None
      if (handshakedPeers.contains(peer)) {
        blacklist(peer.id, blacklistDuration, reason)
      }
      scheduleResume()
  }

  def running: Receive = handleCommonMessages orElse handleBroadcastedBlockMessages orElse handleResponseToRequest orElse {
    case ResumeRegularSync =>
      resumeRegularSync()

    //todo improve mined block handling - add info that block was not included because of syncing [EC-250]
    //we allow inclusion of mined block only if we are not syncing / reorganising chain
    case MinedBlock(block) =>
      if (headersQueue.isEmpty && waitingForActor.isEmpty) {
        //we are at the top of chain we can insert new block
        blockchain.getBlockHeaderByHash(block.header.parentHash)
          .flatMap(b => blockchain.getTotalDifficultyByHash(b.hash)) match {
          case Some(parentTd) if appStateStorage.getBestBlockNumber() < block.header.number =>
            //just insert block and let resolve it with regular download
            val blockProcessResult = processBlock(block, parentTd)
            blockProcessResult match {
              case Right(_) =>
                log.debug(s"Added new mined block $block")
              case Left(err) =>
                log.warning(s"Failed to execute mined block because of $err")
            }
          case _ =>
            log.error("Failed to add mined block")
        }
      } else {
        ommersPool ! AddOmmers(block.header)
      }

    case PrintStatus =>
      log.info(s"Block: ${appStateStorage.getBestBlockNumber()}. Peers: ${handshakedPeers.size} (${blacklistedPeers.size} blacklisted)")
  }

  private def processBlock(block: Block, parentTd: BigInt): Either[BlockExecutionError, BigInt] = {
    val blockHashToDelete = blockchain.getBlockHeaderByNumber(block.header.number).map(_.hash).filter(_ != block.header.hash)
    val blockExecResult = ledger.executeBlock(block, validators)

    blockExecResult.map { receipts =>
      blockchain.save(block)
      blockchain.save(block.header.hash, receipts)
      appStateStorage.putBestBlockNumber(block.header.number)
      val newTd = parentTd + block.header.difficulty
      blockchain.save(block.header.hash, newTd)
      blockHashToDelete.foreach(blockchain.removeBlock)

      broadcastBlock(NewBlock(block, newTd), handshakedPeers)
      ommersPool ! RemoveOmmers((block.header +: block.body.uncleNodesList).toList)
      pendingTransactionsManager ! PendingTransactionsManager.RemoveTransactions(block.body.transactionList)
      newTd
    }
  }

  private def askForHeaders() = {
    bestPeer match {
      case Some(peer) =>
        val blockNumber = appStateStorage.getBestBlockNumber()
        requestBlockHeaders(peer, GetBlockHeaders(Left(blockNumber + 1), blockHeadersPerRequest, skip = 0, reverse = false))
        resolvingBranches = false

      case None =>
        log.debug("No peers to download from")
        scheduleResume()
    }
  }

  private def handleBlockBranchResolution(peer: Peer, message: Seq[BlockHeader]) = {
    if (message.nonEmpty && message.last.hash == headersQueue.head.parentHash) {
      headersQueue = message ++ headersQueue
      processBlockHeaders(peer, headersQueue)
    } else {
      //we did not get previous blocks, there is no way to resolve, blacklist peer and continue download
      resumeWithDifferentPeer(peer)
    }
  }

  private def handleDownload(peer: Peer, message: Seq[BlockHeader]) = if (message.nonEmpty) {
    headersQueue = message
    processBlockHeaders(peer, message)
  } else {
    //no new headers to process, schedule to ask again in future, we are at the top of chain
    scheduleResume()
  }

  private def processBlockHeaders(peer: Peer, headers: Seq[BlockHeader]) = {
    val parentByNumber = blockchain.getBlockHeaderByNumber(headers.head.number - 1)

    parentByNumber match {
      case Some(parent) if checkHeadersChain(headers) =>
        //we have same chain prefix
        if (parent.hash == headers.head.parentHash) {

          val oldBranch: Seq[Block] = getOldBlocks(headersQueue.map(_.number))
          val currentBranchTotalDifficulty: BigInt = oldBranch.map(_.header.difficulty).sum

          val newBranchTotalDifficulty = headersQueue.map(_.difficulty).sum

          if (currentBranchTotalDifficulty < newBranchTotalDifficulty) {
            val transactionsToAdd = oldBranch.flatMap(_.body.transactionList)
            pendingTransactionsManager ! PendingTransactionsManager.AddTransactions(transactionsToAdd.toList)
            val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
            requestBlockBodies(peer, GetBlockBodies(hashes))
            //add first block from branch as ommer
            oldBranch.headOption.foreach { h => ommersPool ! AddOmmers(h.header) }
          } else {
            //add first block from branch as ommer
            headersQueue.headOption.foreach { h => ommersPool ! AddOmmers(h) }
            scheduleResume()
          }
        } else {
          if ((headersQueue.length - 1) / branchResolutionBatchSize >= branchResolutionMaxRequests) {
            log.debug("fail to resolve branch, branch too long, it may indicate malicious peer")
            resumeWithDifferentPeer(peer)
          } else {
            val request = GetBlockHeaders(Right(headersQueue.head.parentHash), branchResolutionBatchSize, skip = 0, reverse = true)
            requestBlockHeaders(peer, request)
            resolvingBranches = true
          }
        }
      case _ =>
        log.debug("Got block header that does not have parent")
        resumeWithDifferentPeer(peer)
    }
  }

  private def requestBlockHeaders(peer: Peer, msg: GetBlockHeaders): Unit = {
    waitingForActor = Some(context.actorOf(
      PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
        peer, peerResponseTimeout, etcPeerManager, peerEventBus,
        requestMsg = msg,
        responseMsgCode = BlockHeaders.code)))
  }

  private def requestBlockBodies(peer: Peer, msg: GetBlockBodies): Unit = {
    waitingForActor = Some(context.actorOf(
      PeerRequestHandler.props[GetBlockBodies, BlockBodies](
        peer, peerResponseTimeout, etcPeerManager, peerEventBus,
        requestMsg = msg,
        responseMsgCode = BlockBodies.code)))
  }

  def getOldBlocks(blockNumbers: Seq[BigInt]): List[Block] = blockNumbers match {
    case Seq(blockNumber, tail @ _*) =>
      blockchain.getBlockByNumber(blockNumber).map(_ :: getOldBlocks(tail)).getOrElse(Nil)
    case Seq() =>
      Nil
  }

  private def handleBlockBodies(peer: Peer, m: Seq[BlockBody]) = {
    if (m.nonEmpty && headersQueue.nonEmpty) {
      val blocks = headersQueue.zip(m).map{ case (header, body) => Block(header, body) }

      blockchain.getBlockHeaderByHash(blocks.head.header.parentHash)
        .flatMap(b => blockchain.getTotalDifficultyByHash(b.hash)) match {
        case Some(blockParentTd) =>
          val (newBlocks, errorOpt) = processBlocks(blocks, blockParentTd)

          if(newBlocks.nonEmpty)
            log.debug(s"got new blocks up till block: ${newBlocks.last.block.header.number} " +
              s"with hash ${Hex.toHexString(newBlocks.last.block.header.hash.toArray[Byte])}")

          errorOpt match {
            case Some(error) =>
              val numberBlockFailed = blocks.head.header.number + newBlocks.length
              resumeWithDifferentPeer(peer, reason = s"a block execution error: ${error.toString}, in block $numberBlockFailed")
            case None =>
              headersQueue = headersQueue.drop(blocks.length)
              if (headersQueue.nonEmpty) {
                val hashes = headersQueue.take(blockBodiesPerRequest).map(_.hash)
                requestBlockBodies(peer, GetBlockBodies(hashes))
              } else {
                context.self ! ResumeRegularSync
              }
          }
        case None =>
          //TODO: Investigate if we can recover from this error (EC-165)
          val parentHash = Hex.toHexString(blocks.head.header.parentHash.toArray)
          log.error(s"No total difficulty for the latest block with number ${blocks.head.header.number - 1} (and hash $parentHash)")
          context stop self
      }

    } else {
      //we got empty response for bodies from peer but we got block headers earlier
      resumeWithDifferentPeer(peer)
    }
  }

  /**
    * Inserts and executes all the blocks, up to the point to which one of them fails (or we run out of blocks).
    * If the execution of any block were to fail, newBlocks only contains the NewBlock msgs for all the blocks executed before it,
    * and only the blocks successfully executed are inserted into the blockchain.
    *
    * @param blocks to execute
    * @param blockParentTd, td of the parent of the blocks.head block
    * @param newBlocks which, after adding the corresponding NewBlock msg for blocks, will be broadcasted
    * @return list of NewBlocks to broadcast (one per block successfully executed) and an error if one happened during execution
    */
  @tailrec
  private def processBlocks(blocks: Seq[Block], blockParentTd: BigInt,
                           newBlocks: Seq[NewBlock] = Nil): (Seq[NewBlock], Option[BlockExecutionError]) = blocks match {
    case Nil =>
      newBlocks -> None

    case Seq(block, otherBlocks@_*) =>
      val blockProcessResult = processBlock(block, blockParentTd)
      blockProcessResult match {
        case Right(newTd) =>
          processBlocks(otherBlocks, newTd, newBlocks :+ NewBlock(block, newTd))
        case Left(error) =>
          newBlocks -> Some(error)
      }
  }

  private def scheduleResume() = {
    headersQueue = Nil
    resumeRegularSyncTimeout = Some(scheduler.scheduleOnce(checkForNewBlockInterval, self, ResumeRegularSync))
  }

  private def resumeWithDifferentPeer(currentPeer: Peer, reason: String = "error in response") = {
    blacklist(currentPeer.id, blacklistDuration, reason)
    headersQueue = Nil
    context.self ! ResumeRegularSync
  }

  private def bestPeer: Option[Peer] = {
    val peersToUse = peersToDownloadFrom
      .collect {
        case (ref, PeerInfo(_, totalDifficulty, true, _)) => (ref, totalDifficulty)
      }

    if (peersToUse.nonEmpty) Some(peersToUse.maxBy { case (_, td) => td }._1)
    else None
  }

}

object RegularSync {
  // scalastyle:off parameter.number
  def props(appStateStorage: AppStateStorage, blockchain: Blockchain, validators: Validators,
            etcPeerManager: ActorRef, peerEventBus: ActorRef, ommersPool: ActorRef, pendingTransactionsManager: ActorRef, ledger: Ledger,
            syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new RegularSync(appStateStorage, blockchain, validators, etcPeerManager, peerEventBus, ommersPool, pendingTransactionsManager,
      ledger, syncConfig, scheduler))

  private[sync] case object ResumeRegularSync
  private case class ResolveBranch(peer: ActorRef)
  private case object PrintStatus

  case object Start
  case class MinedBlock(block: Block)
}
