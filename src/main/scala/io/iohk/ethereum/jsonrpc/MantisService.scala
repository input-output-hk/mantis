package io.iohk.ethereum.jsonrpc
import cats.implicits._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.MantisService.{GetAccountTransactionsRequest, GetAccountTransactionsResponse}
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.transactions.TransactionHistoryService
import io.iohk.ethereum.transactions.TransactionHistoryService.ExtendedTransactionData
import monix.eval.Task

import scala.collection.immutable.NumericRange

object MantisService {
  case class GetAccountTransactionsRequest(address: Address, blocksRange: NumericRange[BigInt])
  case class GetAccountTransactionsResponse(transactions: List[ExtendedTransactionData])
}
class MantisService(transactionHistoryService: TransactionHistoryService, jsonRpcConfig: JsonRpcConfig) {
  def getAccountTransactions(
      request: GetAccountTransactionsRequest
  ): ServiceResponse[GetAccountTransactionsResponse] =
    if (request.blocksRange.length > jsonRpcConfig.accountTransactionsMaxBlocks) {
      Task.now(
        Left(
          JsonRpcError.InvalidParams(
            s"""Maximum number of blocks to search is ${jsonRpcConfig.accountTransactionsMaxBlocks}, requested: ${request.blocksRange.length}.
               |See: 'mantis.network.rpc.account-transactions-max-blocks' config.""".stripMargin
          )
        )
      )
    } else {
      transactionHistoryService
        .getAccountTransactions(request.address, request.blocksRange)
        .map(GetAccountTransactionsResponse(_).asRight)
    }
}
