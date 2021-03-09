package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthUserService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodDecoder}
import org.json4s.JsonAST._

object EthUserJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val eth_getCode: JsonMethodDecoder[GetCodeRequest] with JsonEncoder[GetCodeResponse] = new JsonMethodDecoder[GetCodeRequest] with JsonEncoder[GetCodeResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetCodeRequest] =
      params match {
        case Some(JArray((address: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            addr <- extractAddress(address)
            block <- extractBlockParam(blockValue)
          } yield GetCodeRequest(addr, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetCodeResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getBalance: JsonMethodDecoder[GetBalanceRequest] with JsonEncoder[GetBalanceResponse] = new JsonMethodDecoder[GetBalanceRequest] with JsonEncoder[GetBalanceResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetBalanceRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetBalanceRequest(address, block)
        case other =>
          Left(InvalidParams())
      }

    def encodeJson(t: GetBalanceResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getStorageAt: JsonMethodDecoder[GetStorageAtRequest] with JsonEncoder[GetStorageAtResponse] = new JsonMethodDecoder[GetStorageAtRequest] with JsonEncoder[GetStorageAtResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetStorageAtRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (positionStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            position <- extractQuantity(positionStr)
            block <- extractBlockParam(blockValue)
          } yield GetStorageAtRequest(address, position, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetStorageAtResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getTransactionCount: JsonMethodDecoder[GetTransactionCountRequest] with JsonEncoder[GetTransactionCountResponse] = new JsonMethodDecoder[GetTransactionCountRequest]
    with JsonEncoder[GetTransactionCountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionCountRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetTransactionCountRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetTransactionCountResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getStorageRoot: JsonMethodDecoder[GetStorageRootRequest] with JsonEncoder[GetStorageRootResponse] = new JsonMethodDecoder[GetStorageRootRequest]
    with JsonEncoder[GetStorageRootResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetStorageRootRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetStorageRootRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetStorageRootResponse): JValue = encodeAsHex(t.storageRoot)
  }
}
