package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsRequestHandler.ReceiptsValidationResult
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsRequestHandler.ReceiptsValidationResult._
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Blockchain, Receipt}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}
import io.iohk.ethereum.validators.BlockValidator
import io.iohk.ethereum.validators.BlockValidator.BlockError
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration.FiniteDuration

class FastSyncReceiptsRequestHandler(
    peer: Peer,
    peerResponseTimeout: FiniteDuration,
    etcPeerManager: ActorRef,
    peerMessageBus: ActorRef,
    requestedHashes: Seq[ByteString],
    appStateStorage: AppStateStorage,
    blockchain: Blockchain,
    blockValidator: BlockValidator)(implicit scheduler: Scheduler)
  extends SyncRequestHandler[GetReceipts, Receipts](peer, peerResponseTimeout, etcPeerManager, peerMessageBus) {

  override val requestMsg = GetReceipts(requestedHashes)
  override val responseMsgCode: Int = Receipts.code

  override def handleResponseMsg(receipts: Receipts): Unit = {
    validateReceipts(requestedHashes, receipts) match {
      case ReceiptsValid(blockHashesWithReceipts) =>
        blockHashesWithReceipts.foreach { case (hash, receiptsForBlock) =>
          blockchain.save(hash, receiptsForBlock)
        }

        val receivedHashes = blockHashesWithReceipts.unzip._1
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
    * Validates whether the received receipts match the block headers stored on the blockchain,
    * returning the valid receipts
    *
    * @param requestedHashes hash of the blocks to which the requested receipts should belong
    * @param receipts received by the peer
    * @return the valid receipts or the error encountered while validating them
    */
  private def validateReceipts(requestedHashes: Seq[ByteString], receipts: Receipts): ReceiptsValidationResult = {
    val blockHashesWithReceipts = requestedHashes.zip(receipts.receiptsForBlocks)
    val blockHeadersWithReceipts = blockHashesWithReceipts.map{ case (hash, blockReceipts) =>
      blockchain.getBlockHeaderByHash(hash) -> blockReceipts }

    val receiptsValidationError = blockHeadersWithReceipts.collectFirst {
      case (Some(header), receipt) if blockValidator.validateBlockAndReceipts(header, receipt).isLeft =>
        ReceiptsInvalid(blockValidator.validateBlockAndReceipts(header, receipt).left.get)
      case (None, _) => ReceiptsDbError
    }
    receiptsValidationError match {
      case Some(error) => error
      case None => ReceiptsValid(blockHashesWithReceipts)
    }
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
  def props(peer: Peer, peerTimeout: FiniteDuration, etcPeerManager: ActorRef, peerMessageBus: ActorRef, requestedHashes: Seq[ByteString],
            appStateStorage: AppStateStorage, blockchain: Blockchain, blockValidator: BlockValidator)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncReceiptsRequestHandler(
      peer, peerTimeout, etcPeerManager, peerMessageBus, requestedHashes, appStateStorage, blockchain, blockValidator))

  sealed trait ReceiptsValidationResult
  object ReceiptsValidationResult {
    case class ReceiptsValid(blockHashesAndReceipts: Seq[(ByteString, Seq[Receipt])]) extends ReceiptsValidationResult
    case class ReceiptsInvalid(error: BlockError) extends ReceiptsValidationResult
    case object ReceiptsDbError extends ReceiptsValidationResult
  }
}
