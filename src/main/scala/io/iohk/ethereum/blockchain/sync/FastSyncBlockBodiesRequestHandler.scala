package io.iohk.ethereum.blockchain.sync

import akka.actor.{ActorRef, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncController.BlockBodiesReceived
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, GetBlockBodies}

class FastSyncBlockBodiesRequestHandler(
    peer: ActorRef,
    requestedHashes: Seq[ByteString],
    appStateStorage: AppStateStorage)(implicit scheduler: Scheduler)
  extends FastSyncRequestHandler[GetBlockBodies, BlockBodies](peer) {

  override val requestMsg = GetBlockBodies(requestedHashes)
  override val responseMsgCode: Int = BlockBodies.code

  override def handleResponseMsg(blockBodies: BlockBodies): Unit = {
    if (blockBodies.bodies.isEmpty) {
      fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    }

    fastSyncController ! BlockBodiesReceived(peer, requestedHashes, blockBodies.bodies)

    log.info("Received {} block bodies in {} ms", blockBodies.bodies.size, timeTakenSoFar())
    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    fastSyncController ! SyncController.EnqueueBlockBodies(requestedHashes)
    cleanupAndStop()
  }

  override def handleTerminated(): Unit = {
    fastSyncController ! SyncController.EnqueueBlockBodies(requestedHashes)
    cleanupAndStop()
  }

}

object FastSyncBlockBodiesRequestHandler {
  def props(peer: ActorRef, requestedHashes: Seq[ByteString], appStateStorage: AppStateStorage)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncBlockBodiesRequestHandler(peer, requestedHashes, appStateStorage))
}
