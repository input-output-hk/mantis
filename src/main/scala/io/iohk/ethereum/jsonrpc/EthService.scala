package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.jsonrpc.EthService.SyncingResponse

import scala.concurrent.Future

object EthService {
  case class SyncingResponse(startingBlock: BigInt, currentBlock: BigInt, highestBlock: BigInt)
}

class EthService(appStateStorage: AppStateStorage) {

  def syncing(): Future[SyncingResponse] = {
    Future.successful(SyncingResponse(
      startingBlock = appStateStorage.getSyncStartingBlock(),
      currentBlock = appStateStorage.getBestBlockNumber(),
      highestBlock = appStateStorage.getEstimatedHighestBlock()))
  }

}
