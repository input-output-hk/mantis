package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}

class FastSyncReceiptsRequestHandler(
    peer: ActorRef,
    requestedHashes: Seq[ByteString],
    blockchain: Blockchain)(implicit scheduler: Scheduler)
  extends FastSyncRequestHandler[GetReceipts, Receipts](peer) {

  override val requestMsg = GetReceipts(requestedHashes)
  override val responseMsgCode = Receipts.code

  override def handleResponseMsg(receipts: Receipts): Unit = {
    (requestedHashes zip receipts.receiptsForBlocks).foreach { case (hash, receiptsForBlock) =>
      blockchain.save(hash, receiptsForBlock)
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

  override def handleTerminated(): Unit = {
    fastSyncController ! FastSyncController.EnqueueReceipts(requestedHashes)
    cleanupAndStop()
  }
}

object FastSyncReceiptsRequestHandler {
  def props(peer: ActorRef, requestedHashes: Seq[ByteString], blockchain: Blockchain)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncReceiptsRequestHandler(peer, requestedHashes, blockchain))
}
