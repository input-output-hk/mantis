package io.iohk.ethereum.blockchain.sync

import akka.actor.{Scheduler, Props, ActorRef}
import akka.util.ByteString
import io.iohk.ethereum.db.storage.BlockBodiesStorage
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, GetBlockBodies}

class FastSyncBlockBodiesRequestHandler(
    peer: ActorRef,
    requestedHashes: Seq[ByteString],
    blockBodiesStorage: BlockBodiesStorage)(implicit scheduler: Scheduler)
  extends FastSyncRequestHandler[GetBlockBodies, BlockBodies](peer) {

  override val requestMsg = GetBlockBodies(requestedHashes)
  override val responseMsgCode = BlockBodies.code

  override def handleResponseMsg(blockBodies: BlockBodies): Unit = {
    (requestedHashes zip blockBodies.bodies).foreach { case (hash, body) =>
      blockBodiesStorage.put(hash, body)
    }

    if (blockBodies.bodies.isEmpty) {
      fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    }

    val remainingBlockBodies = requestedHashes.drop(blockBodies.bodies.size)
    if (remainingBlockBodies.nonEmpty) {
      fastSyncController ! FastSyncController.EnqueueBlockBodies(remainingBlockBodies)
    }

    log.info("Received {} block bodies in {} ms", blockBodies.bodies.size, timeTakenSoFar())
    cleanupAndStop()
  }

  override def handleTimeout(): Unit = {
    fastSyncController ! BlacklistSupport.BlacklistPeer(peer)
    fastSyncController ! FastSyncController.EnqueueBlockBodies(requestedHashes)
    cleanupAndStop()
  }

  override def handleTerminated(): Unit = {
    fastSyncController ! FastSyncController.EnqueueBlockBodies(requestedHashes)
    cleanupAndStop()
  }

}

object FastSyncBlockBodiesRequestHandler {
  def props(peer: ActorRef, requestedHashes: Seq[ByteString], blockBodiesStorage: BlockBodiesStorage)
           (implicit scheduler: Scheduler): Props =
    Props(new FastSyncBlockBodiesRequestHandler(peer, requestedHashes, blockBodiesStorage))
}
