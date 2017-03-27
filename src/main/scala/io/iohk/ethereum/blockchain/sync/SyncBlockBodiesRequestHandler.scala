package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncController.BlockBodiesReceived
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, GetBlockBodies}
import org.spongycastle.util.encoders.Hex

class SyncBlockBodiesRequestHandler(
    peer: ActorRef,
    requestedHashes: Seq[ByteString])(implicit scheduler: Scheduler)
  extends SyncRequestHandler[GetBlockBodies, BlockBodies](peer) {

  override val requestMsg = GetBlockBodies(requestedHashes)
  override val responseMsgCode: Int = BlockBodies.code

  override def handleResponseMsg(blockBodies: BlockBodies): Unit = {
    if (blockBodies.bodies.isEmpty) {
      val reason = s"got empty block bodies response for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
      syncController ! BlacklistSupport.BlacklistPeer(peer, reason)
    } else {
      syncController ! BlockBodiesReceived(peer, requestedHashes, blockBodies.bodies)
    }

    log.info("Received {} block bodies in {} ms", blockBodies.bodies.size, timeTakenSoFar())
    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    val reason = s"time out on block bodies response for known hashes: ${requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))}"
    syncController ! BlacklistSupport.BlacklistPeer(peer, reason)
    syncController ! FastSync.EnqueueBlockBodies(requestedHashes)
    cleanupAndStop()
  }

  override def handleTerminated(): Unit = {
    syncController ! FastSync.EnqueueBlockBodies(requestedHashes)
    cleanupAndStop()
  }

}

object SyncBlockBodiesRequestHandler {
  def props(peer: ActorRef, requestedHashes: Seq[ByteString])
           (implicit scheduler: Scheduler): Props =
    Props(new SyncBlockBodiesRequestHandler(peer, requestedHashes))
}
