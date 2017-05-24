package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}

class SyncBlockHeadersRequestHandler(
    peer: Peer,
    peerMessageBus: ActorRef,
    val requestMsg: GetBlockHeaders,
    resolveBranches: Boolean)(implicit scheduler: Scheduler)
  extends SyncRequestHandler[GetBlockHeaders, BlockHeaders](peer, peerMessageBus) {

  override val responseMsgCode: Int = BlockHeaders.code

  override def handleResponseMsg(blockHeaders: BlockHeaders): Unit = {
    val headers = if (requestMsg.reverse)
      blockHeaders.headers.reverse
     else
      blockHeaders.headers

    val consistentHeaders = checkHeaders(headers)

    if (consistentHeaders) {
      if (resolveBranches) {
        syncController ! SyncController.BlockHeadersToResolve(peer, headers)
        log.info("Received {} block headers in {} ms", headers.size, timeTakenSoFar())
      } else {
        val blockHashes = headers.map(_.hash)
        syncController ! SyncController.BlockHeadersReceived(peer, headers)
        syncController ! FastSync.EnqueueBlockBodies(blockHashes)
        syncController ! FastSync.EnqueueReceipts(blockHashes)
        log.info("Received {} block headers in {} ms", headers.size, timeTakenSoFar())
      }
    } else {
      val reason = s"got error in block headers response for requested: ${requestMsg.block}"
      syncController ! BlacklistSupport.BlacklistPeer(peer.id, reason)
    }

    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    val reason = s"got time out waiting for block headers response for requested: ${requestMsg.block}"
    syncController ! BlacklistSupport.BlacklistPeer(peer.id, reason)
    cleanupAndStop()
  }

  override def handleTerminated(): Unit = {
    cleanupAndStop()
  }

  private def checkHeaders(headers: Seq[BlockHeader]): Boolean = if (headers.length > 1) {
    headers.zip(headers.tail).forall { case (parent, child) => parent.hash == child.parentHash && parent.number + 1 == child.number }
  } else {
    true
  }
}

object SyncBlockHeadersRequestHandler {
  def props(peer: Peer, peerMessageBus: ActorRef, requestMsg: GetBlockHeaders, resolveBranches: Boolean)
           (implicit scheduler: Scheduler): Props =
    Props(new SyncBlockHeadersRequestHandler(peer, peerMessageBus, requestMsg, resolveBranches))
}
