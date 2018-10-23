package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorLogging
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.blockchain.sync.FastSyncBlocksValidator.BlockBodyValidationResult
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

import scala.concurrent.duration.FiniteDuration

trait FastSyncBlockBodiesHandler extends FastSyncBlocksValidator with FastSyncFixtures { this: ActorLogging =>

  def handleBlockBodies(
    peer: Peer,
    requestedHashes: Seq[ByteString],
    blockBodies: Seq[BlockBody],
    handlerState: FastSyncHandlerState,
    blacklist: (BlackListId, FiniteDuration, String) => Unit
  ): FastSyncHandlerState = {
    if (blockBodies.isEmpty) {
      val reason = s"got empty block bodies response for known hashes: ${hashes2strings(requestedHashes)}"
      blacklist(peer.id, syncConfig.blacklistDuration, reason)
      handlerState.withEnqueueBlockBodies(requestedHashes)
    } else {
      validateBlocks(requestedHashes, blockBodies) match {
        case BlockBodyValidationResult.Valid   =>
          insertBlocks(requestedHashes, blockBodies, handlerState)

        case BlockBodyValidationResult.Invalid =>
          val reason = s"responded with block bodies not matching block headers, blacklisting for ${syncConfig.blacklistDuration}"
          blacklist(peer.id, syncConfig.blacklistDuration, reason)
          handlerState.withEnqueueBlockBodies(requestedHashes)

        case BlockBodyValidationResult.DbError =>
          restartDownload(handlerState)
      }
    }
  }

  private def insertBlocks(requestedHashes: Seq[ByteString], blockBodies: Seq[BlockBody], handlerState: FastSyncHandlerState): FastSyncHandlerState = {
    (requestedHashes zip blockBodies).foreach { case (hash, body) => blockchain.save(hash, body) }

    val (toUpdate, remaining) = requestedHashes.splitAt(blockBodies.size)
    updateBestBlockIfNeeded(toUpdate)
    if (remaining.nonEmpty) {
      handlerState.withEnqueueBlockBodies(remaining)
    } else {
      handlerState
    }
  }
}
