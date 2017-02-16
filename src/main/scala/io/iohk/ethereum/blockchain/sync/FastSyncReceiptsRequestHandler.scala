package io.iohk.ethereum.blockchain.sync

import akka.actor.{Scheduler, Props, ActorRef}
import akka.util.ByteString
import io.iohk.ethereum.db.storage.ReceiptStorage
import io.iohk.ethereum.network.p2p.messages.PV63.{Receipts, GetReceipts}

class FastSyncReceiptsRequestHandler(
    peer: ActorRef,
    requestedHashes: Seq[ByteString],
    receiptStorage: ReceiptStorage)(implicit scheduler: Scheduler)
  extends FastSyncRequestHandler[GetReceipts, Receipts](peer) {

  override val requestMsg = GetReceipts(requestedHashes)
  override val responseMsgCode = Receipts.code

  override def handleResponseMsg(receipts: Receipts): Unit = {
    (requestedHashes zip receipts.receiptsForBlocks).foreach { case (hash, receiptsForBlock) =>
      receiptStorage.put(hash, receiptsForBlock)
    }

    if (receipts.receiptsForBlocks.isEmpty) {
      fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    }

    val remainingReceipts = requestedHashes.drop(receipts.receiptsForBlocks.size)
    if (remainingReceipts.nonEmpty) {
      fastSyncController ! FastSyncController.EnqueueReceipts(remainingReceipts)
    }

    log.info("Received {} receipts in {} ms", receipts.receiptsForBlocks.size, timeTakenSoFar())
    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    fastSyncController ! FastSyncController.EnqueueReceipts(requestedHashes)
    cleanupAndStop()
  }
}

object FastSyncReceiptsRequestHandler {
  def props(peer: ActorRef, requestedHashes: Seq[ByteString], receiptStorage: ReceiptStorage)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncReceiptsRequestHandler(peer, requestedHashes, receiptStorage))
}
