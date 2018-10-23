package io.iohk.ethereum.blockchain.sync

import akka.actor.ActorLogging
import akka.util.ByteString
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{ Block, Blockchain }
import io.iohk.ethereum.utils.Config.SyncConfig
import org.bouncycastle.util.encoders.Hex

trait FastSyncFixtures { this: ActorLogging =>

  def appStateStorage: AppStateStorage
  def blockchain: Blockchain
  def syncConfig: SyncConfig

  def updateBestBlockIfNeeded(receivedHashes: Seq[ByteString]): Unit = {
    val fullBlocks = receivedHashes.flatMap { hash =>
      for {
        Block(header, _) <- blockchain.getBlockByHash(hash)
        _ <- blockchain.getReceiptsByHash(hash)
      } yield header
    }

    if (fullBlocks.nonEmpty) {
      val bestReceivedBlock = fullBlocks.maxBy(_.number).number
      if (appStateStorage.getBestBlockNumber() < bestReceivedBlock) appStateStorage.putBestBlockNumber(bestReceivedBlock)
    }
  }

  /** Restarts download from a few blocks behind the current best block header, as an unexpected DB error happened */
  def restartDownload(handlerState: FastSyncHandlerState): FastSyncHandlerState = {
    log.debug("Missing block header for known hash")
    val syncState = handlerState.syncState
    handlerState.withSyncState(syncState.copy(
      blockBodiesQueue = Nil,
      receiptsQueue = Nil,
      //todo adjust the formula to minimize redownloaded block headers
      bestBlockHeaderNumber = (syncState.bestBlockHeaderNumber - 2 * syncConfig.blockHeadersPerRequest).max(0)
    ))
  }

  def hashes2strings(requestedHashes: Seq[ByteString]): Seq[String] =
    requestedHashes.map(h => Hex.toHexString(h.toArray[Byte]))

}
