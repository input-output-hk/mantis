package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import org.json4s.{DefaultFormats, Formats, JValue}
import org.json4s.JsonAST.{JArray, JString}
import org.json4s.JsonDSL._
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

object JsonMethodsImplicits {

  import JsonRpcErrors._

  implicit val formats: Formats = DefaultFormats

  implicit val web3_sha3 = new JsonDecoder[Sha3Request] with JsonEncoder[Sha3Response] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, Sha3Request] =
      params match {
        case Some(JArray((input: JString) :: Nil)) if input.s.startsWith("0x") =>
          Try(ByteString(Hex.decode(input.s.drop(2)))) match {
            case Success(bs) => Right(Sha3Request(bs))
            case Failure(_) => Left(InvalidParams)
          }

        case _ => Left(InvalidParams)
      }

    override def encodeJson(t: Sha3Response): JValue =
      JString(s"0x${Hex.toHexString(t.data.toArray[Byte])}")
  }

  implicit val web3_clientVersion = new JsonDecoder[ClientVersionRequest] with JsonEncoder[ClientVersionResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, ClientVersionRequest] = Right(ClientVersionRequest())

    override def encodeJson(t: ClientVersionResponse): JValue = t.value
  }

}
