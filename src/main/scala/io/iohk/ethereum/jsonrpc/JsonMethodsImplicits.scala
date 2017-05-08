package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import org.json4s.{CustomSerializer, DefaultFormats, Formats, JsonAST}
import org.json4s.JsonAST.{JArray, JString}
import org.json4s.JsonDSL._
import org.spongycastle.util.encoders.Hex

import scala.util.{Success, Try}

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

  object ByteStringSerializer extends CustomSerializer[ByteString](formats =>
    (
      { case JString(str) => ByteString(Hex.decode(str.drop(2)))},
      { case bs: ByteString => JString(s"0x${Hex.toHexString(bs.toArray[Byte])}") }
    ))
}
