package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsRequestHandler.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsRequestHandler.ReceiptsValidationResult._
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}
import io.iohk.ethereum.validators.BlockValidator
import io.iohk.ethereum.validators.BlockValidator.BlockError
import org.spongycastle.util.encoders.Hex

class FastSyncReceiptsRequestHandler(
    peer: Peer,
    etcPeerManager: ActorRef,
    peerMessageBus: ActorRef,
    requestedHashes: Seq[ByteString],
    appStateStorage: AppStateStorage,
    blockchain: Blockchain,
    blockValidator: BlockValidator)(implicit scheduler: Scheduler)
  extends SyncRequestHandler[GetReceipts, Receipts](peer, etcPeerManager, peerMessageBus) {

  override val requestMsg = GetReceipts(requestedHashes)
  override val responseMsgCode: Int = Receipts.code

  override def handleResponseMsg(receipts: Receipts): Unit = {
    validateReceipts(requestedHashes, receipts) match {
      case ReceiptsValid =>
        (requestedHashes zip receipts.receiptsForBlocks).foreach { case (hash, receiptsForBlock) =>
          blockchain.save(hash, receiptsForBlock)
        }

        val receivedHashes = requestedHashes.take(receipts.receiptsForBlocks.size)
        updateBestBlockIfNeeded(receivedHashes)

        if (receipts.receiptsForBlocks.isEmpty) {
          val reason = s"got empty receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
          syncController ! BlacklistSupport.BlacklistPeer(peer.id, reason)
        }

        val remainingReceipts = requestedHashes.drop(receipts.receiptsForBlocks.size)
        if (remainingReceipts.nonEmpty) {
          syncController ! FastSync.EnqueueReceipts(remainingReceipts)
        }

        log.info("Received {} receipts in {} ms", receipts.receiptsForBlocks.size, timeTakenSoFar())

      case ReceiptsInvalid(error) =>
        val reason =
          s"got invalid receipts for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}" +
          s" due to: $error"
        syncController ! BlacklistSupport.BlacklistPeer(peer.id, reason)
        syncController ! FastSync.EnqueueReceipts(requestedHashes)

      case ReceiptsDbError =>
        syncController ! FastSync.RedownloadBlockchain
    }
    cleanupAndStop()
  }

  private def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
    val fullBlocks = receivedHashes.flatMap { hash =>
      for {
        header <- blockchain.getBlockHeaderByHash(hash)
        _ <- blockchain.getBlockBodyByHash(hash)
      } yield header
    }

    if (fullBlocks.nonEmpty) {
      val bestReceivedBlock = fullBlocks.maxBy(_.number)
      if (bestReceivedBlock.number > appStateStorage.getBestBlockNumber()) {
        appStateStorage.putBestBlockNumber(bestReceivedBlock.number)
      }
    }
  }

  /**
    * Validates whether the received receipts match the block headers stored on the blockchain
    *
    * @param requestedHashes hash of the blocks to which the requested receipts should belong
    * @param receipts received by the peer
    * @return whether the receipts are valid or not
    */
  private def validateReceipts(requestedHashes: Seq[ByteString], receipts: Receipts): ReceiptsValidationResult = {
    val expectedAnsweredHashes = requestedHashes.take(receipts.receiptsForBlocks.size)
    val blockHeaders = expectedAnsweredHashes.flatMap(hash => blockchain.getBlockHeaderByHash(hash))

    if(blockHeaders.size == expectedAnsweredHashes.size) {
      val receiptValidationResult: Option[BlockError] = blockHeaders.zip(receipts.receiptsForBlocks)
        .collectFirst{case (header, receipt) if blockValidator.validateBlockAndReceipts(header, receipt).isLeft =>
          blockValidator.validateBlockAndReceipts(header, receipt).left.get
        }

      receiptValidationResult match {
        case Some(error) => ReceiptsInvalid(error)
        case None => ReceiptsValid
      }
    } else
      ReceiptsDbError
  }

  override def handleTimeout(): Unit = {
    val reason = s"time out on receipts response for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
    syncController ! BlacklistSupport.BlacklistPeer(peer.id, reason)
    syncController ! FastSync.EnqueueReceipts(requestedHashes)
    cleanupAndStop()
  }

  override def handleTerminated(): Unit = {
    syncController ! FastSync.EnqueueReceipts(requestedHashes)
    cleanupAndStop()
  }
}

object FastSyncReceiptsRequestHandler {
  def props(peer: Peer, etcPeerManager: ActorRef, peerMessageBus: ActorRef, requestedHashes: Seq[ByteString],
            appStateStorage: AppStateStorage, blockchain: Blockchain, blockValidator: BlockValidator)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncReceiptsRequestHandler(
      peer, etcPeerManager, peerMessageBus, requestedHashes, appStateStorage, blockchain, blockValidator))

  sealed trait ReceiptsValidationResult
  object ReceiptsValidationResult {
    case object ReceiptsValid extends ReceiptsValidationResult
    case class ReceiptsInvalid(error: BlockError) extends ReceiptsValidationResult
    case object ReceiptsDbError extends ReceiptsValidationResult
  }
}
