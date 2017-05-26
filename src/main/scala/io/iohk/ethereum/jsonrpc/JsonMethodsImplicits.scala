package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.PersonalService._
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

  protected def tryExtractUnformattedData(input: JString): Either[JsonRpcError, ByteString] =
    tryExtractUnformattedData(input.values)

  protected def tryExtractUnformattedData(input: String): Either[JsonRpcError, ByteString] = {
    if (input.startsWith("0x")) {
      Try(ByteString(Hex.decode(input.drop(2)))) match {
        case Success(bs) => Right(bs)
        case Failure(_) => Left(InvalidParams(s"Unable to parse data from '$input'"))
      }
    } else Left(InvalidParams(s"Data '$input' should have 0x prefix"))
  }

  protected def tryExtractQuantity(input: JString): Either[JsonRpcError, BigInt] =
    tryExtractQuantity(input.values)

  protected def tryExtractQuantity(input: String): Either[JsonRpcError, BigInt] = {
    if (input.startsWith("0x")) {
      val noPrefix = input.replace("0x", "")
      Try(BigInt(noPrefix, 16)) match {
        case Success(bi) => Right(bi)
        case Failure(_) => Left(InvalidParams(s"Unable to parse quantity from '$input'"))
      }
    } else Left(InvalidParams(s"Quantity '$input' should have 0x prefix"))
  }

  protected def tryExtractBlockParam(input: JString): Either[JsonRpcError, BlockParam] = {
    input.values match {
      case "earliest" => Right(BlockParam.Earliest)
      case "latest" => Right(BlockParam.Latest)
      case "pending" => Right(BlockParam.Pending)
      case other =>
        tryExtractQuantity(other) match {
          case Right(n) => Right(BlockParam.WithNumber(n))
          case Left(_) => Try(BlockParam.WithNumber(BigInt(other)))
            .toEither.left.map(_ => JsonRpcErrors.InvalidParams(s"Invalid default block param: $other"))
        }
    }
  }

}

object JsonMethodsImplicits extends JsonMethodsImplicits {

  import JsonRpcErrors._

  implicit val web3_sha3 = new JsonDecoder[Sha3Request] with JsonEncoder[Sha3Response] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, Sha3Request] =
      params match {
        case Some(JArray((input: JString) :: Nil)) => tryExtractUnformattedData(input).map(Sha3Request)
        case _ => Left(InvalidParams())
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

  implicit val personal_importRawKey = new JsonDecoder[ImportRawKeyRequest] with JsonEncoder[ImportRawKeyResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ImportRawKeyRequest] =
      params match {
        case Some(JArray(JString(key) :: JString(passphrase) :: _)) =>
          Right(ImportRawKeyRequest(key, passphrase))
        case _ =>
          Left(InvalidParams())
      }

    def encodeJson(t: ImportRawKeyResponse): JValue =
      JString(t.address)
  }

  implicit val personal_newAccount = new JsonDecoder[NewAccountRequest] with JsonEncoder[NewAccountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewAccountRequest] =
      params match {
        case Some(JArray(JString(passphrase) :: _)) =>
          Right(NewAccountRequest(passphrase))
        case _ =>
          Left(InvalidParams())
      }

    def encodeJson(t: NewAccountResponse): JValue =
      JString(t.address)
  }

  implicit val personal_listAccounts = new JsonDecoder[ListAccountsRequest] with JsonEncoder[ListAccountsResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ListAccountsRequest] =
      Right(ListAccountsRequest())

    def encodeJson(t: ListAccountsResponse): JValue =
      JArray(t.addresses.map(JString))
  }
}
