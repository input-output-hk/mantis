package io.iohk.ethereum.blockchain.sync.fast

import akka.event.LoggingAdapter
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration.FiniteDuration

trait FastSyncReceiptsHandler extends FastSyncReceiptsValidator {

  def syncConfig: SyncConfig
  def log: LoggingAdapter

  def handleReceipts(
    peer: Peer,
    requestedHashes: Seq[ByteString],
    receipts: Seq[Seq[Receipt]],
    blacklist: (BlackListId, FiniteDuration, String) => Unit,
    updateBestBlock: Seq[ByteString] => Unit
  ): Option[Seq[ByteString]] = {
    lazy val knownHashes = requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))
    validateReceipts(requestedHashes, receipts) match {
      case FastSyncReceiptsValidator.Valid(blockHashesWithReceipts) =>
        blockHashesWithReceipts foreach { case (hash, receiptsForBlock) => blockchain.save(hash, receiptsForBlock)}

        val (receivedHashes, _) = blockHashesWithReceipts.unzip
        updateBestBlock(receivedHashes)

        if (receipts.isEmpty) {
          val reason = s"got empty receipts for known hashes: $knownHashes"
          blacklist(peer.id, syncConfig.blacklistDuration, reason)
        }

        val remainingReceipts = requestedHashes.drop(receipts.size)
        Some(remainingReceipts)

      case FastSyncReceiptsValidator.Invalid(error) =>
        val reason = s"got invalid receipts for known hashes: $knownHashes due to: $error"
        blacklist(peer.id, syncConfig.blacklistDuration, reason)
        Some(requestedHashes)

      case FastSyncReceiptsValidator.DbError =>
        log.debug("Missing block header for known hash")
        None
    }
  }
}
