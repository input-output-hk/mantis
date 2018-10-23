package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorLogging
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.blockchain.sync.FastSyncReceiptsValidator.ReceiptsValidationResult
import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.network.Peer

import scala.concurrent.duration.FiniteDuration

trait FastSyncReceiptsHandler extends FastSyncFixtures with FastSyncReceiptsValidator { this: ActorLogging =>

  def handleReceipts(
    peer: Peer,
    requestedHashes: Seq[ByteString],
    receipts: Seq[Seq[Receipt]],
    handlerState: FastSyncHandlerState,
    blacklist: (BlackListId, FiniteDuration, String) => Unit
  ): FastSyncHandlerState = {
    lazy val knownHashes = hashes2strings(requestedHashes)
    validateReceipts(requestedHashes, receipts) match {
      case ReceiptsValidationResult.Valid(blockHashesWithReceipts) =>
        blockHashesWithReceipts.foreach { case (hash, receiptsForBlock) =>
          blockchain.save(hash, receiptsForBlock)
        }

        val (receivedHashes, _) = blockHashesWithReceipts.unzip
        updateBestBlockIfNeeded(receivedHashes)

        if (receipts.isEmpty) {
          val reason = s"got empty receipts for known hashes: $knownHashes"
          blacklist(peer.id,syncConfig.blacklistDuration, reason)
        }

        val remainingReceipts = requestedHashes.drop(receipts.size)
        if (remainingReceipts.nonEmpty) {
          handlerState.withEnqueueReceipts(remainingReceipts)
        } else {
          handlerState
        }

      case ReceiptsValidationResult.Invalid(error) =>
        val reason = s"got invalid receipts for known hashes: $knownHashes due to: $error"
        blacklist(peer.id, syncConfig.blacklistDuration, reason)
        handlerState.withEnqueueReceipts(requestedHashes)

      case ReceiptsValidationResult.DbError =>
        restartDownload(handlerState)
    }
  }
}
