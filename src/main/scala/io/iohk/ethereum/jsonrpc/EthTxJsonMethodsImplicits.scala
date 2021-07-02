package io.iohk.ethereum.jsonrpc

import org.json4s.Extraction
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

import io.iohk.ethereum.jsonrpc.EthTxService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder.OptionToNull._
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder

object EthTxJsonMethodsImplicits extends JsonMethodsImplicits {
  implicit val transactionResponseJsonEncoder: JsonEncoder[TransactionResponse] = Extraction.decompose(_)

  implicit val eth_gasPrice: NoParamsMethodDecoder[GetGasPriceRequest] with JsonEncoder[GetGasPriceResponse] =
    new NoParamsMethodDecoder(GetGasPriceRequest()) with JsonEncoder[GetGasPriceResponse] {
      override def encodeJson(t: GetGasPriceResponse): JValue = encodeAsHex(t.price)
    }

  implicit val eth_pendingTransactions
      : NoParamsMethodDecoder[EthPendingTransactionsRequest] with JsonEncoder[EthPendingTransactionsResponse] =
    new NoParamsMethodDecoder(EthPendingTransactionsRequest()) with JsonEncoder[EthPendingTransactionsResponse] {

      override def encodeJson(t: EthPendingTransactionsResponse): JValue =
        JArray(t.pendingTransactions.toList.map { pendingTx =>
          encodeAsHex(pendingTx.stx.tx.hash)
        })
    }

  implicit val eth_getTransactionByHash
      : JsonMethodDecoder[GetTransactionByHashRequest] with JsonEncoder[GetTransactionByHashResponse] =
    new JsonMethodDecoder[GetTransactionByHashRequest] with JsonEncoder[GetTransactionByHashResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByHashRequest] =
        params match {
          case Some(JArray(JString(txHash) :: Nil)) =>
            for {
              parsedTxHash <- extractHash(txHash)
            } yield GetTransactionByHashRequest(parsedTxHash)
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: GetTransactionByHashResponse): JValue =
        JsonEncoder.encode(t.txResponse)
    }

  implicit val eth_getTransactionReceipt
      : JsonMethodDecoder[GetTransactionReceiptRequest] with JsonEncoder[GetTransactionReceiptResponse] =
    new JsonMethodDecoder[GetTransactionReceiptRequest] with JsonEncoder[GetTransactionReceiptResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionReceiptRequest] =
        params match {
          case Some(JArray(JString(txHash) :: Nil)) =>
            for {
              parsedTxHash <- extractHash(txHash)
            } yield GetTransactionReceiptRequest(parsedTxHash)
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: GetTransactionReceiptResponse): JValue =
        Extraction.decompose(t.txResponse)
    }

  implicit val GetTransactionByBlockHashAndIndexResponseEncoder
      : JsonEncoder[GetTransactionByBlockHashAndIndexResponse] =
    new JsonEncoder[GetTransactionByBlockHashAndIndexResponse] {
      override def encodeJson(t: GetTransactionByBlockHashAndIndexResponse): JValue =
        JsonEncoder.encode(t.transactionResponse)
    }

  implicit val GetTransactionByBlockHashAndIndexRequestDecoder
      : JsonMethodDecoder[GetTransactionByBlockHashAndIndexRequest] =
    new JsonMethodDecoder[GetTransactionByBlockHashAndIndexRequest] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByBlockHashAndIndexRequest] =
        params match {
          case Some(JArray(JString(blockHash) :: transactionIndex :: Nil)) =>
            for {
              parsedBlockHash <- extractHash(blockHash)
              parsedTransactionIndex <- extractQuantity(transactionIndex)
            } yield GetTransactionByBlockHashAndIndexRequest(parsedBlockHash, parsedTransactionIndex)
          case _ => Left(InvalidParams())
        }
    }

  implicit val GetTransactionByBlockNumberAndIndexResponseEncoder
      : JsonEncoder[GetTransactionByBlockNumberAndIndexResponse] =
    new JsonEncoder[GetTransactionByBlockNumberAndIndexResponse] {
      override def encodeJson(t: GetTransactionByBlockNumberAndIndexResponse): JValue =
        JsonEncoder.encode(t.transactionResponse)
    }

  implicit val GetTransactionByBlockNumberAndIndexRequestDecoder
      : JsonMethodDecoder[GetTransactionByBlockNumberAndIndexRequest] =
    new JsonMethodDecoder[GetTransactionByBlockNumberAndIndexRequest] {
      override def decodeJson(
          params: Option[JArray]
      ): Either[JsonRpcError, GetTransactionByBlockNumberAndIndexRequest] =
        params match {
          case Some(JArray(blockParam :: transactionIndex :: Nil)) =>
            for {
              blockParam <- extractBlockParam(blockParam)
              parsedTransactionIndex <- extractQuantity(transactionIndex)
            } yield GetTransactionByBlockNumberAndIndexRequest(blockParam, parsedTransactionIndex)
          case _ => Left(InvalidParams())
        }
    }

  implicit val eth_sendRawTransaction
      : JsonMethodDecoder[SendRawTransactionRequest] with JsonEncoder[SendRawTransactionResponse] =
    new JsonMethodDecoder[SendRawTransactionRequest] with JsonEncoder[SendRawTransactionResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendRawTransactionRequest] =
        params match {
          case Some(JArray(JString(dataStr) :: Nil)) =>
            for {
              data <- extractBytes(dataStr)
            } yield SendRawTransactionRequest(data)
          case _ => Left(InvalidParams())
        }

      def encodeJson(t: SendRawTransactionResponse): JValue = encodeAsHex(t.transactionHash)
    }

  implicit val RawTransactionResponseJsonEncoder: JsonEncoder[RawTransactionResponse] =
    new JsonEncoder[RawTransactionResponse] {
      override def encodeJson(t: RawTransactionResponse): JValue =
        t.transactionResponse.map((RawTransactionCodec.asRawTransaction _).andThen(encodeAsHex))
    }

}
