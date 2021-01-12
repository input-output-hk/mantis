package io.iohk.ethereum.jsonrpc

import java.time.Duration

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.jsonrpc.EthService.BlockParam
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.PersonalService._
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec, JsonMethodDecoder, JsonSerializers}
import io.iohk.ethereum.utils.BigIntExtensionMethods.BigIntAsUnsigned
import org.bouncycastle.util.encoders.Hex
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

import scala.util.Try

trait JsonMethodsImplicits {
  implicit val formats = JsonSerializers.formats

  def encodeAsHex(input: ByteString): JString =
    JString(s"0x${Hex.toHexString(input.toArray[Byte])}")

  def encodeAsHex(input: Byte): JString =
    JString(s"0x${Hex.toHexString(Array(input))}")

  def encodeAsHex(input: BigInt): JString =
    JString(s"0x${input.toString(16)}")

  protected def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    Hex.decode(normalized)
  }

  protected def extractAddress(input: String): Either[JsonRpcError, Address] =
    Try(Address(input)).toEither.left.map(_ => InvalidAddress)

  protected def extractDurationQuantity(input: JValue): Either[JsonRpcError, Duration] = for {
    quantity <- extractQuantity(input)
    duration <- getDuration(quantity)
  } yield duration

  private def getDuration(value: BigInt): Either[JsonRpcError, Duration] = {
    Either.cond(
      value.isValidInt,
      Duration.ofSeconds(value.toInt),
      InvalidParams("Duration should be an number of seconds, less than 2^31 - 1")
    )
  }

  protected def extractAddress(input: JString): Either[JsonRpcError, Address] =
    extractAddress(input.s)

  protected def extractBytes(input: String): Either[JsonRpcError, ByteString] =
    Try(ByteString(decode(input))).toEither.left.map(_ => InvalidParams())

  protected def extractBytes(input: JString): Either[JsonRpcError, ByteString] =
    extractBytes(input.s)

  protected def extractBytes(input: String, size: Int): Either[JsonRpcError, ByteString] =
    extractBytes(input).filterOrElse(_.length == size, InvalidParams(s"Invalid value [$input], expected $size bytes"))

  protected def extractHash(input: String): Either[JsonRpcError, ByteString] =
    extractBytes(input, 32)

  protected def extractQuantity(input: JValue): Either[JsonRpcError, BigInt] =
    input match {
      case JInt(n) =>
        Right(n)

      case JString(s) =>
        Try(BigInt(1, decode(s))).toEither.left.map(_ => InvalidParams())

      case _ =>
        Left(InvalidParams("could not extract quantity"))
    }

  protected def optionalQuantity(input: JValue): Either[JsonRpcError, Option[BigInt]] =
    input match {
      case JNothing => Right(None)
      case o => extractQuantity(o).map(Some(_))
    }

  protected def extractTx(input: Map[String, JValue]): Either[JsonRpcError, TransactionRequest] = {
    def optionalQuantity(name: String): Either[JsonRpcError, Option[BigInt]] = input.get(name) match {
      case Some(v) => extractQuantity(v).map(Some(_))
      case None => Right(None)
    }

    for {
      from <- input.get("from") match {
        case Some(JString(s)) if s.nonEmpty => extractAddress(s)
        case Some(_) => Left(InvalidAddress)
        case _ => Left(InvalidParams("TX 'from' is required"))
      }

      to <- input.get("to") match {
        case Some(JString(s)) if s.nonEmpty => extractAddress(s).map(Option.apply)
        case Some(JString(_)) => extractAddress("0x0").map(Option.apply)
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
        extractQuantity(other)
          .map(BlockParam.WithNumber)
          .left
          .map(_ => JsonRpcError.InvalidParams(s"Invalid default block param: $other"))
    }
  }

  def toEitherOpt[A, B](opt: Option[Either[A, B]]): Either[A, Option[B]] =
    opt.map(_.map(Some.apply)).getOrElse(Right(None))

}

object JsonMethodsImplicits extends JsonMethodsImplicits {

  import JsonRpcError._

  implicit val web3_sha3 = new JsonMethodDecoder[Sha3Request] with JsonEncoder[Sha3Response] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, Sha3Request] =
      params match {
        case Some(JArray((input: JString) :: Nil)) => extractBytes(input).map(Sha3Request)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: Sha3Response): JValue = encodeAsHex(t.data)
  }

  implicit val web3_clientVersion = new NoParamsMethodDecoder(ClientVersionRequest())
    with JsonEncoder[ClientVersionResponse] {
    override def encodeJson(t: ClientVersionResponse): JValue = t.value
  }

  implicit val net_version = new NoParamsMethodDecoder(VersionRequest()) with JsonEncoder[VersionResponse] {
    override def encodeJson(t: VersionResponse): JValue = t.value
  }

  implicit val net_listening = new NoParamsMethodDecoder(ListeningRequest()) with JsonEncoder[ListeningResponse] {
    override def encodeJson(t: ListeningResponse): JValue = t.value
  }

  implicit val net_peerCount = new NoParamsMethodDecoder(PeerCountRequest()) with JsonEncoder[PeerCountResponse] {
    override def encodeJson(t: PeerCountResponse): JValue = encodeAsHex(t.value)
  }

  implicit val personal_importRawKey = new JsonMethodDecoder[ImportRawKeyRequest]
    with JsonEncoder[ImportRawKeyResponse] {
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

  implicit val personal_newAccount = new JsonMethodDecoder[NewAccountRequest] with JsonEncoder[NewAccountResponse] {
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

  implicit val personal_listAccounts = new NoParamsMethodDecoder(ListAccountsRequest())
    with JsonEncoder[ListAccountsResponse] {
    def encodeJson(t: ListAccountsResponse): JValue =
      JArray(t.addresses.map(a => JString(a.toString)))
  }

  implicit val personal_sendTransaction =
    new JsonMethodCodec[SendTransactionWithPassphraseRequest, SendTransactionWithPassphraseResponse] {
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

  implicit val personal_sign = new JsonMethodCodec[SignRequest, SignResponse] {
    override def encodeJson(t: SignResponse): JValue = {
      import t.signature._
      encodeAsHex(ByteString(r.toUnsignedByteArray ++ s.toUnsignedByteArray :+ v))
    }

    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, SignRequest] =
      params match {
        case Some(JArray(JString(message) :: JString(addr) :: JString(passphase) :: _)) =>
          for {
            message <- extractBytes(message)
            address <- extractAddress(addr)
          } yield SignRequest(message, address, Some(passphase))
        case _ =>
          Left(InvalidParams())
      }
  }

  implicit val personal_ecRecover = new JsonMethodCodec[EcRecoverRequest, EcRecoverResponse] {

    def decodeJson(params: Option[JArray]): Either[JsonRpcError, EcRecoverRequest] =
      params match {
        case Some(JArray(JString(message) :: JString(signature) :: _)) =>
          val decoded = for {
            msg <- extractBytes(message)
            sig <- extractBytes(signature, ECDSASignature.EncodedLength)
          } yield (msg, sig)

          decoded.flatMap { case (msg, sig) =>
            val r = sig.take(ECDSASignature.RLength)
            val s = sig.drop(ECDSASignature.RLength).take(ECDSASignature.SLength)
            val v = sig.last

            if (ECDSASignature.allowedPointSigns.contains(v)) {
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

  implicit val personal_unlockAccount = new JsonMethodCodec[UnlockAccountRequest, UnlockAccountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, UnlockAccountRequest] = {
      params match {
        case Some(JArray(JString(addr) :: JString(passphrase) :: JNull :: _)) =>
          extractAddress(addr).map(UnlockAccountRequest(_, passphrase, None))
        case Some(JArray(JString(addr) :: JString(passphrase) :: duration :: _)) =>
          for {
            addr <- extractAddress(addr)
            duration <- extractDurationQuantity(duration)
          } yield UnlockAccountRequest(addr, passphrase, Some(duration))
        case Some(JArray(JString(addr) :: JString(passphrase) :: _)) =>
          extractAddress(addr).map(UnlockAccountRequest(_, passphrase, None))
        case _ =>
          Left(InvalidParams())
      }
    }

    def encodeJson(t: UnlockAccountResponse): JValue =
      JBool(t.result)
  }

  implicit val personal_lockAccount = new JsonMethodCodec[LockAccountRequest, LockAccountResponse] {
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
