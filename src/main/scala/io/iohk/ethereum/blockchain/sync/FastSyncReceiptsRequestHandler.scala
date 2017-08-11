package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV63.{GetReceipts, Receipts}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration.FiniteDuration

class FastSyncReceiptsRequestHandler(
    peer: Peer,
    peerResponseTimeout: FiniteDuration,
    etcPeerManager: ActorRef,
    peerMessageBus: ActorRef,
    requestedHashes: Seq[ByteString],
    appStateStorage: AppStateStorage,
    blockchain: Blockchain)(implicit scheduler: Scheduler)
  extends SyncRequestHandler[GetReceipts, Receipts](peer, peerResponseTimeout, etcPeerManager, peerMessageBus) {

  override val requestMsg = GetReceipts(requestedHashes)
  override val responseMsgCode: Int = Receipts.code

  override def handleResponseMsg(receipts: Receipts): Unit = {
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
  def props(peer: Peer, peerTimeout: FiniteDuration, etcPeerManager: ActorRef, peerMessageBus: ActorRef,
            requestedHashes: Seq[ByteString], appStateStorage: AppStateStorage, blockchain: Blockchain)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncReceiptsRequestHandler(peer, peerTimeout, etcPeerManager, peerMessageBus, requestedHashes, appStateStorage, blockchain))
}
