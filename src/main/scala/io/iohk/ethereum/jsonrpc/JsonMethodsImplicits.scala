package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.Web3Service._
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JValue, JsonAST}
import org.json4s.JsonAST.{JArray, JBool, JString}
import org.json4s.JsonDSL._
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

object JsonMethodsImplicits {

  implicit val formats: Formats = DefaultFormats + ByteStringSerializer

  implicit val web3_sha3 = new JsonDecoder[Sha3Request] with JsonEncoder[Sha3Response] {
    override def decodeJson(params: JsonAST.JArray): Try[Sha3Request] =
      Try(Sha3Request(params.arr.head.extract[ByteString]))

    override def encodeJson(t: Sha3Response): JsonAST.JArray = {
      JArray(List(JString(s"0x${Hex.toHexString(t.data.toArray[Byte])}")))
    }
  }

  implicit val web3_clientVersion = new JsonDecoder[ClientVersionRequest] with JsonEncoder[ClientVersionResponse] {
    override def decodeJson(params: JsonAST.JArray): Try[ClientVersionRequest] = Success(ClientVersionRequest())

    override def encodeJson(t: ClientVersionResponse): JsonAST.JArray = JArray(List(t.value))
  }

  implicit val eth_submitHashrate = new JsonDecoder[HashRateSubmitRequest] with JsonEncoder[HashRateSubmitResponse] {
    override def decodeJson(params: JsonAST.JArray): Try[HashRateSubmitRequest] = params match {
      case JArray(hashRate :: id :: Nil) =>
        Success(HashRateSubmitRequest(hashRate.extract[ByteString], id.extract[ByteString]))
      case _ =>
        Failure(new RuntimeException(s"got malformed HashRateSubmitRequest, JSON: $params"))
    }

    override def encodeJson(t: HashRateSubmitResponse): JValue = JBool(t.success)
  }

  implicit val eth_getWork = new JsonDecoder[GetWorkRequest] with JsonEncoder[GetWorkResponse] {
    override def decodeJson(params: JArray): Try[GetWorkRequest] =
      Success(GetWorkRequest())

    override def encodeJson(t: GetWorkResponse): JsonAST.JValue ={
      val powHeaderHash = s"0x${Hex.toHexString(t.powHeaderHash.toArray[Byte])}"
      val dagSeed = s"0x${Hex.toHexString(t.dagSeed.toArray[Byte])}"
      val target = s"0x${Hex.toHexString(t.target.toArray[Byte])}"
      JArray(List(powHeaderHash, dagSeed, target))
    }
  }

  object ByteStringSerializer extends CustomSerializer[ByteString](formats =>
    (
      { case JString("0x0") => ByteString("0x00")
        case JString(str) => ByteString(Hex.decode(str.drop(2)))},
      { case bs: ByteString => JString(s"0x${Hex.toHexString(bs.toArray[Byte])}") }
    ))
}
