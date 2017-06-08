package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.jsonrpc.JsonSerializers.{OptionNoneToJNullSerializer, QuantitiesSerializer, UnformattedDataJsonSerializer}
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.{DefaultFormats, Formats}
import org.spongycastle.util.encoders.Hex

import scala.util.Try

trait JsonMethodsImplicits {

  trait Codec[Req, Res] extends JsonDecoder[Req] with JsonEncoder[Res]

  implicit val formats: Formats = DefaultFormats.preservingEmptyValues + OptionNoneToJNullSerializer +
    QuantitiesSerializer + UnformattedDataJsonSerializer

  protected def encodeAsHex(input: ByteString): JString =
    JString(s"0x${Hex.toHexString(input.toArray[Byte])}")

  protected def encodeAsHex(input: BigInt): JString =
    JString(s"0x${input.toString(16)}")

  private def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    Hex.decode(normalized)
  }

  protected def extractAddress(input: String): Either[JsonRpcError, Address] =
    Try(Address(input)).toEither.left.map(_ => InvalidAddress)

  protected def extractAddress(input: JString): Either[JsonRpcError, Address] =
    extractAddress(input.s)

  protected def extractBytes(input: String): Either[JsonRpcError, ByteString] =
    Try(ByteString(decode(input))).toEither.left.map(_ => InvalidParams())

  protected def extractBytes(input: JString): Either[JsonRpcError, ByteString] =
    extractBytes(input.s)

  protected def extractHash(input: String): Either[JsonRpcError, ByteString] =
    extractBytes(input).filterOrElse(_.length == 32, InvalidParams(s"Invalid value [$input], expected 32 bytes"))

  protected def extractQuantity(input: JValue): Either[JsonRpcError, BigInt] =
    input match {
      case JInt(n) =>
        Right(n)

      case JString(s) =>
        Try(BigInt(decode(s))).toEither.left.map(_ => InvalidParams())

      case _ =>
        Left(InvalidParams("could not extract quantity"))
    }

  protected def extractTx(input: Map[String, JValue]): Either[JsonRpcError, TransactionRequest] = {
    def optionalQuantity(name: String): Either[JsonRpcError, Option[BigInt]] = input.get(name) match {
      case Some(v) => extractQuantity(v).map(Some(_))
      case None => Right(None)
    }

    for {
      from <- input.get("from") match {
        case Some(JString(s)) => extractAddress(s)
        case Some(_) => Left(InvalidAddress)
        case _ => Left(InvalidParams("TX 'from' is required"))
      }

      to <- input.get("to") match {
        case Some(JString(s)) => extractAddress(s).map(Some(_))
        case Some(_) => Left(InvalidAddress)
        case None => Right(None)
      }

      value <- optionalQuantity("value")

      gas <- optionalQuantity("gas")

      gasPrice <- optionalQuantity("gasPrice")

      nonce <- optionalQuantity("nonce")

      data <- input.get("data") match {
        case Some(JString(s)) => extractBytes(s).map(Some(_))
        case Some(_) => Left(InvalidParams())
        case None => Right(None)
      }
    } yield TransactionRequest(from, to, value, gas, gasPrice, nonce, data)
  }

  protected def extractBlockParam(input: JValue): Either[JsonRpcError, BlockParam] = {
    input match {
      case JString("earliest") => Right(BlockParam.Earliest)
      case JString("latest") => Right(BlockParam.Latest)
      case JString("pending") => Right(BlockParam.Pending)
      case other =>
        extractQuantity(other).map(BlockParam.WithNumber)
          .left.map(_ => JsonRpcErrors.InvalidParams(s"Invalid default block param: $other"))
    }
  }
}

object JsonMethodsImplicits extends JsonMethodsImplicits {

  import JsonRpcErrors._

  implicit val web3_sha3 = new JsonDecoder[Sha3Request] with JsonEncoder[Sha3Response] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, Sha3Request] =
      params match {
        case Some(JArray((input: JString) :: Nil)) => extractBytes(input).map(Sha3Request)
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
          extractBytes(key).map(ImportRawKeyRequest(_, passphrase))
        case _ =>
          Left(InvalidParams())
      }

    def encodeJson(t: ImportRawKeyResponse): JValue =
      JString(t.address.toString)
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
      JString(t.address.toString)
  }

  implicit val personal_listAccounts = new JsonDecoder[ListAccountsRequest] with JsonEncoder[ListAccountsResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ListAccountsRequest] =
      Right(ListAccountsRequest())

    def encodeJson(t: ListAccountsResponse): JValue =
      JArray(t.addresses.map(a => JString(a.toString)))
  }

  implicit val personal_sendTransaction = new Codec[SendTransactionWithPassphraseRequest, SendTransactionWithPassphraseResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendTransactionWithPassphraseRequest] =
      params match {
        case Some(JArray(JObject(tx) :: JString(passphrase) :: _)) =>
          extractTx(tx.toMap).map(SendTransactionWithPassphraseRequest(_, passphrase))
        case _ =>
          Left(InvalidParams())
      }

    def encodeJson(t: SendTransactionWithPassphraseResponse): JValue =
      encodeAsHex(t.txHash)
  }

  implicit val personal_ecRecover = new Codec[EcRecoverRequest, EcRecoverResponse] {
    val signatureLength = 65
    val rLength = 32
    val vLength = 32

    def decodeJson(params: Option[JArray]): Either[JsonRpcError, EcRecoverRequest] =
      params match {
        case Some(JArray(JString(message) :: JString(signature) :: _)) if signature.length == (signatureLength * 2 + 2) =>

          val decoded = for {
            msg <- extractBytes(message)
            sig <- extractBytes(signature)
          } yield (msg, sig)

          decoded.flatMap { case (msg, sig) =>
            val r = sig.take(rLength)
            val s = sig.drop(rLength).take(vLength)
            val v = sig.takeRight(1)

            if (v.contains(ECDSASignature.positivePointSign) || v.contains(ECDSASignature.negativePointSign)) {
              Right(EcRecoverRequest(msg, ECDSASignature(r, s, v)))
            } else {
              Left(InvalidParams("invalid point sign v, allowed values are 27 and 28"))
            }
          }
        case _ =>
          Left(InvalidParams())
      }

    def encodeJson(t: EcRecoverResponse): JValue =
      encodeAsHex(t.address.bytes)
  }

  implicit val personal_unlockAccount = new Codec[UnlockAccountRequest, UnlockAccountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, UnlockAccountRequest] = {
      params match {
        case Some(JArray(JString(addr) :: JString(passphrase) :: _)) =>
          extractAddress(addr).map(UnlockAccountRequest(_, passphrase))
        case _ =>
          Left(InvalidParams())
      }
    }

    def encodeJson(t: UnlockAccountResponse): JValue =
      JBool(t.result)
  }

  implicit val personal_lockAccount = new Codec[LockAccountRequest, LockAccountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, LockAccountRequest] = {
      params match {
        case Some(JArray(JString(addr) :: _)) =>
          extractAddress(addr).map(LockAccountRequest)
        case _ =>
          Left(InvalidParams())
      }
    }

    def encodeJson(t: LockAccountResponse): JValue =
      JBool(t.result)
  }
}
