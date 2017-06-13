package io.iohk.ethereum

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{Block, Blockchain}
import io.iohk.ethereum.jsonrpc.EthService.BlockParam

import scala.concurrent.Future

package object jsonrpc {
  type ServiceResponse[T] = Future[Either[JsonRpcError, T]]

  def resolveBlock(blockParam: BlockParam, blockchain: Blockchain, appStateStorage: AppStateStorage): Either[JsonRpcError, Block] = {
    def getBlock(number: BigInt): Either[JsonRpcError, Block] = {
      blockchain.getBlockByNumber(number)
        .map(Right.apply)
        .getOrElse(Left(JsonRpcErrors.InvalidParams(s"Block $number not found")))
    }

    getBlock(resolveBlockNumber(blockParam, appStateStorage))
  }

  def resolveBlockNumber(blockParam: BlockParam, appStateStorage: AppStateStorage): BigInt = {
    blockParam match {
      case BlockParam.WithNumber(blockNumber) => blockNumber
      case BlockParam.Earliest => 0
      case BlockParam.Latest => appStateStorage.getBestBlockNumber()
      case BlockParam.Pending => appStateStorage.getBestBlockNumber()
    }
  }

}
