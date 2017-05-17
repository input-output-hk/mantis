package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import org.json4s.JsonAST.{JArray, JString, JValue}
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, Formats}
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

trait JsonMethodsImplicits {

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  protected def encodeAsHex(input: ByteString): JString =
    JString(s"0x${Hex.toHexString(input.toArray[Byte])}")

  protected def encodeAsHex(input: BigInt): JString =
    JString(s"0x${input.toString(16)}")

  protected def tryExtractUnformattedData(input: JString): Either[JsonRpcError, ByteString] = {
    if (input.s.startsWith("0x")) {
      Try(ByteString(Hex.decode(input.s.drop(2)))) match {
        case Success(bs) => Right(bs)
        case Failure(_) => Left(InvalidParams.copy(message = s"Unable to parse data from '${input.s}'"))
      }
    } else Left(InvalidParams.copy(message = s"Data '${input.s}' should have 0x prefix"))
  }

  protected def tryExtractQuantity(input: JString): Either[JsonRpcError, BigInt] = {
    if (input.s.startsWith("0x")) {
      val noPrefix = input.s.replace("0x", "")
      Try(BigInt(noPrefix, 16)) match {
        case Success(bi) => Right(bi)
        case Failure(_) => Left(InvalidParams.copy(message = s"Unable to parse quantity from '${input.s}'"))
      }
    } else Left(InvalidParams.copy(message = s"Quantity '${input.s}' should have 0x prefix"))
  }
}

object JsonMethodsImplicits extends JsonMethodsImplicits {

  import JsonRpcErrors._

  implicit val web3_sha3 = new JsonDecoder[Sha3Request] with JsonEncoder[Sha3Response] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, Sha3Request] =
      params match {
        case Some(JArray((input: JString) :: Nil)) => tryExtractUnformattedData(input).map(Sha3Request)
        case _ => Left(InvalidParams)
      }

    override def encodeJson(t: Sha3Response): JValue = encodeAsHex(t.data)
  }

  implicit val web3_clientVersion = new JsonDecoder[ClientVersionRequest] with JsonEncoder[ClientVersionResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, ClientVersionRequest] = Right(ClientVersionRequest())
    override def encodeJson(t: ClientVersionResponse): JValue = t.value
  }

  implicit val net_version = new JsonDecoder[VersionRequest] with JsonEncoder[VersionResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, VersionRequest] = Right(VersionRequest())
    override def encodeJson(t: VersionResponse): JValue = t.value
  }

  implicit val net_listening = new JsonDecoder[ListeningRequest] with JsonEncoder[ListeningResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, ListeningRequest] = Right(ListeningRequest())
    override def encodeJson(t: ListeningResponse): JValue = t.value
  }

  implicit val net_peerCount = new JsonDecoder[PeerCountRequest] with JsonEncoder[PeerCountResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, PeerCountRequest] = Right(PeerCountRequest())
    override def encodeJson(t: PeerCountResponse): JValue = encodeAsHex(t.value)
  }
}
