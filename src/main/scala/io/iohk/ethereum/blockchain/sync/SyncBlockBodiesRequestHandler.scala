package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncController.BlockBodiesReceived
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, GetBlockBodies}

class SyncBlockBodiesRequestHandler(
    peer: ActorRef,
    requestedHashes: Seq[ByteString])(implicit scheduler: Scheduler)
  extends SyncRequestHandler[GetBlockBodies, BlockBodies](peer) {

  override val requestMsg = GetBlockBodies(requestedHashes)
  override val responseMsgCode: Int = BlockBodies.code

  override def handleResponseMsg(blockBodies: BlockBodies): Unit = {
    if (blockBodies.bodies.isEmpty) {
      syncController ! BlacklistSupport.BlacklistPeer(peer)
    } else {
      syncController ! BlockBodiesReceived(peer, requestedHashes, blockBodies.bodies)
    }

    log.info("Received {} block bodies in {} ms", blockBodies.bodies.size, timeTakenSoFar())
    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    syncController ! BlacklistSupport.BlacklistPeer(peer)
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
