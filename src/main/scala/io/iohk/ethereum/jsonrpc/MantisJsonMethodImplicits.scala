package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthTxJsonMethodsImplicits.transactionResponseJsonEncoder
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.MantisService.{GetAccountTransactionsRequest, GetAccountTransactionsResponse}
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder.Ops._
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec, JsonMethodDecoder}
import io.iohk.ethereum.transactions.TransactionHistoryService.ExtendedTransactionData
import org.json4s.JsonAST._
import org.json4s.Merge
import JsonEncoder.OptionToNull._

object MantisJsonMethodImplicits extends JsonMethodsImplicits {
  implicit val extendedTransactionDataJsonEncoder: JsonEncoder[ExtendedTransactionData] = extendedTxData => {
    val asTxResponse = TransactionResponse(
      extendedTxData.stx,
      extendedTxData.minedTransactionData.map(_.header),
      extendedTxData.minedTransactionData.map(_.transactionIndex)
    )

    val encodedTxResponse = JsonEncoder.encode(asTxResponse)
    val encodedExtension = JObject(
      "isOutgoing" -> extendedTxData.isOutgoing.jsonEncoded,
      "isCheckpointed" -> extendedTxData.minedTransactionData.map(_.isCheckpointed).jsonEncoded,
      "isPending" -> extendedTxData.isPending.jsonEncoded,
      "gasUsed" -> extendedTxData.minedTransactionData.map(_.gasUsed).jsonEncoded,
      "timestamp" -> extendedTxData.minedTransactionData.map(_.timestamp).jsonEncoded
    )

    Merge.merge(encodedTxResponse, encodedExtension)
  }

  implicit val mantis_getAccountTransactions
      : JsonMethodCodec[GetAccountTransactionsRequest, GetAccountTransactionsResponse] =
    new JsonMethodDecoder[GetAccountTransactionsRequest] with JsonEncoder[GetAccountTransactionsResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetAccountTransactionsRequest] =
        params match {
          case Some(JArray(JString(addrJson) :: fromBlockJson :: toBlockJson :: Nil)) =>
            for {
              addr <- extractAddress(addrJson)
              fromBlock <- extractQuantity(fromBlockJson)
              toBlock <- extractQuantity(toBlockJson)
            } yield GetAccountTransactionsRequest(addr, fromBlock to toBlock)
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: GetAccountTransactionsResponse): JValue =
        JObject("transactions" -> t.transactions.jsonEncoded)
    }
}
