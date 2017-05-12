package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.db.storage.AppStateStorage

import scala.concurrent.Future


object EthService {
  val CurrentProtocolVersion = 63

  case class ProtocolVersionRequest()
  case class ProtocolVersionResponse(value: String)

  case class SyncingRequest()
  case class SyncingResponse(startingBlock: BigInt, currentBlock: BigInt, highestBlock: BigInt)
}

class EthService(appStateStorage: AppStateStorage) {
  import EthService._

  def protocolVersion(req: ProtocolVersionRequest): Future[ProtocolVersionResponse] =
    Future.successful(ProtocolVersionResponse(f"0x$CurrentProtocolVersion%x"))

  def syncing(req: SyncingRequest): Future[SyncingResponse] = {
    Future.successful(SyncingResponse(
      startingBlock = appStateStorage.getSyncStartingBlock(),
      currentBlock = appStateStorage.getBestBlockNumber(),
      highestBlock = appStateStorage.getEstimatedHighestBlock()))
  }

}
