package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.jsonrpc.EthService.SyncingResponse

import scala.concurrent.Future

object EthService {
  case class SyncingResponse()
}

class EthService(appStateStorage: AppStateStorage) {

  def syncing(): Future[SyncingResponse] = {

    if (appStateStorage.isFastSyncDone()) {
      "return false"
      // best block spoko
      // estimated chujnia
      // starting block spoko
    } else {
      val bestBlock = appStateStorage.getBestBlockNumber()
      val estimated = appStateStorage.getEstimatedHighestBlock()
      val startingBlock = appStateStorage.getSyncStartingBlock()
    }

  }

}
