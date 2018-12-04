package io.iohk.ethereum.blockchain.sync.fast

import akka.event.LoggingAdapter
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration.FiniteDuration

trait FastSyncBlockBodiesHandler extends FastSyncBlockBodiesValidator {

  def syncConfig: SyncConfig
  def log: LoggingAdapter

  def handleBlockBodies(
    peer: Peer,
    requestedHashes: Seq[ByteString],
    blockBodies: Seq[BlockBody],
    blacklist: (BlackListId, FiniteDuration, String) => Unit,
    updateBestBlock: Seq[ByteString] => Unit
  ): Option[Seq[ByteString]] = {
    if (blockBodies.isEmpty) {
      val hashes = requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))
      val reason = s"got empty block bodies response for known hashes: $hashes"
      blacklist(peer.id, syncConfig.blacklistDuration, reason)
      Some(requestedHashes)
    } else {
      validateBlocks(requestedHashes, blockBodies) match {
        case FastSyncBlockBodiesValidator.Valid   =>
          (requestedHashes zip blockBodies) foreach { case (hash, body) => blockchain.save(hash, body) }

          val (received, remaining) = requestedHashes.splitAt(blockBodies.size)
          updateBestBlock(received)
          Some(remaining)

        case FastSyncBlockBodiesValidator.Invalid =>
          val reason = s"responded with block bodies not matching block headers, blacklisting for ${syncConfig.blacklistDuration}"
          blacklist(peer.id, syncConfig.blacklistDuration, reason)
          Some(requestedHashes)

        case FastSyncBlockBodiesValidator.DbError =>
          log.debug("Missing block header for known hash")
          None
      }
    }
  }
}
