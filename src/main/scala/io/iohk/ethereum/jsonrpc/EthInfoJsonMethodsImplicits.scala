package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import org.json4s.Extraction
import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

import io.iohk.ethereum.jsonrpc.EthInfoService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.PersonalService.SendTransactionRequest
import io.iohk.ethereum.jsonrpc.PersonalService.SendTransactionResponse
import io.iohk.ethereum.jsonrpc.PersonalService.SignRequest
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodCodec
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder

object EthJsonMethodsImplicits extends JsonMethodsImplicits {
  implicit val eth_chainId: NoParamsMethodDecoder[ChainIdRequest] with JsonEncoder[ChainIdResponse] =
    new NoParamsMethodDecoder(ChainIdRequest()) with JsonEncoder[ChainIdResponse] {
      def encodeJson(t: ChainIdResponse) = encodeAsHex(t.value)
    }

  implicit val eth_protocolVersion
      : NoParamsMethodDecoder[ProtocolVersionRequest] with JsonEncoder[ProtocolVersionResponse] =
    new NoParamsMethodDecoder(ProtocolVersionRequest()) with JsonEncoder[ProtocolVersionResponse] {
      def encodeJson(t: ProtocolVersionResponse): JValue = t.value
    }

  implicit val eth_syncing: NoParamsMethodDecoder[SyncingRequest] with JsonEncoder[SyncingResponse] =
    new NoParamsMethodDecoder(SyncingRequest()) with JsonEncoder[SyncingResponse] {
      def encodeJson(t: SyncingResponse): JValue = t.syncStatus match {
        case Some(syncStatus) => Extraction.decompose(syncStatus)
        case None             => false
      }
    }

  implicit val eth_sendTransaction: JsonMethodCodec[SendTransactionRequest, SendTransactionResponse] =
    new JsonMethodCodec[SendTransactionRequest, SendTransactionResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendTransactionRequest] =
        params match {
          case Some(JArray(JObject(tx) :: _)) =>
            extractTx(tx.toMap).map(SendTransactionRequest)
          case _ =>
            Left(InvalidParams())
        }

      def encodeJson(t: SendTransactionResponse): JValue =
        encodeAsHex(t.txHash)
    }

  implicit val eth_call: JsonMethodDecoder[CallRequest] with JsonEncoder[CallResponse] =
    new JsonMethodDecoder[CallRequest] with JsonEncoder[CallResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, CallRequest] =
        params match {
          case Some(JArray((txObj: JObject) :: (blockValue: JValue) :: Nil)) =>
            for {
              blockParam <- extractBlockParam(blockValue)
              tx <- extractCall(txObj)
            } yield CallRequest(tx, blockParam)
          case _ => Left(InvalidParams())
        }

      def encodeJson(t: CallResponse): JValue = encodeAsHex(t.returnData)
    }

  implicit val eth_estimateGas: eth_estimateGas = new eth_estimateGas
  class eth_estimateGas extends JsonMethodDecoder[CallRequest] with JsonEncoder[EstimateGasResponse] {
    override def encodeJson(t: EstimateGasResponse): JValue = encodeAsHex(t.gas)

    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, CallRequest] =
      withoutBlockParam.applyOrElse(params, eth_call.decodeJson)

    def withoutBlockParam: PartialFunction[Option[JArray], Either[JsonRpcError, CallRequest]] = {
      case Some(JArray((txObj: JObject) :: Nil)) =>
        extractCall(txObj).map(CallRequest(_, BlockParam.Latest))
    }

  }

  implicit val eth_sign: JsonMethodDecoder[SignRequest] = new JsonMethodDecoder[SignRequest] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, SignRequest] =
      params match {
        case Some(JArray(JString(addr) :: JString(message) :: _)) =>
          for {
            message <- extractBytes(message)
            address <- extractAddress(addr)
          } yield SignRequest(message, address, None)
        case _ =>
          Left(InvalidParams())
      }
  }

  def extractCall(obj: JObject): Either[JsonRpcError, CallTx] = {
    def toEitherOpt[A, B](opt: Option[Either[A, B]]): Either[A, Option[B]] =
      opt match {
        case Some(Right(v)) => Right(Option(v))
        case Some(Left(e))  => Left(e)
        case None           => Right(None)
      }

    for {
      from <- toEitherOpt((obj \ "from").extractOpt[String].map(extractBytes))
      to <- toEitherOpt((obj \ "to").extractOpt[String].map(extractBytes))
      gas <- optionalQuantity(obj \ "gas")
      gasPrice <- optionalQuantity(obj \ "gasPrice")
      value <- optionalQuantity(obj \ "value")
      data <- toEitherOpt((obj \ "data").extractOpt[String].map(extractBytes))
    } yield CallTx(
      from = from,
      to = to,
      gas = gas,
      gasPrice = gasPrice.getOrElse(0),
      value = value.getOrElse(0),
      data = data.getOrElse(ByteString(""))
    )
  }

}
