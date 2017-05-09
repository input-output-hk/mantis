package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import io.iohk.ethereum.jsonrpc.Web3Service._
import org.json4s.{DefaultFormats, Formats, JValue, JsonAST}
import org.json4s.JsonAST.{JArray, JBool, JString}
import org.json4s.JsonDSL._
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

object JsonMethodsImplicits {

  import JsonRpcErrors._

  implicit val formats: Formats = DefaultFormats

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

  implicit val eth_submitHashrate = new JsonDecoder[SubmitHashRateRequest] with JsonEncoder[SubmitHashRateResponse] {
    override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitHashRateRequest] = params match {
      case Some(JArray((hashRate: JString) :: (id: JString) :: Nil)) =>
        tryExtractQuantity(hashRate)
          .flatMap(h => tryExtractUnformattedData(id).map(i => (h, i)))
          .map { case (h, i) => SubmitHashRateRequest(h, i) }
      case _ =>
        Left(InvalidParams)
    }

    override def encodeJson(t: SubmitHashRateResponse): JValue = JBool(t.success)
  }

  implicit val eth_getWork = new JsonDecoder[GetWorkRequest] with JsonEncoder[GetWorkResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetWorkRequest] = params match {
      case None => Right(GetWorkRequest())
      case Some(_) => Left(InvalidParams)
    }

    override def encodeJson(t: GetWorkResponse): JsonAST.JValue ={
      val powHeaderHash = s"0x${Hex.toHexString(t.powHeaderHash.toArray[Byte])}"
      val dagSeed = s"0x${Hex.toHexString(t.dagSeed.toArray[Byte])}"
      val target = s"0x${Hex.toHexString(t.target.toArray[Byte])}"
      JArray(List(powHeaderHash, dagSeed, target))
    }
  }

  implicit val eth_submitWork = new JsonDecoder[SubmitWorkRequest] with JsonEncoder[SubmitWorkResponse] {
    override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitWorkRequest] = params match {
      case Some(JArray((nonce: JString) :: (powHeaderHash: JString) :: (mixHash: JString) :: Nil)) =>
        tryExtractUnformattedData(nonce)
          .flatMap(n => tryExtractUnformattedData(powHeaderHash).map(p => (n, p)))
          .flatMap { case (n, p) => tryExtractUnformattedData(mixHash).map(m => (n, p, m)) }
          .map { case (n, p, m) => SubmitWorkRequest(n, p, m) }
      case _ =>
        Left(InvalidParams)
    }

    override def encodeJson(t: SubmitWorkResponse): JValue = JBool(t.success)
  }

  private def encodeAsHex(input: ByteString): JString =
    JString(s"0x${Hex.toHexString(input.toArray[Byte])}")

  private def encodeAsHex(input: BigInt): JString =
    JString(s"0x${Hex.toHexString(input.toByteArray)}")

  private def tryExtractUnformattedData(input: JString): Either[JsonRpcError, ByteString] = {
    if (input.s.startsWith("0x")) {
      Try(ByteString(Hex.decode(input.s.drop(2)))) match {
        case Success(bs) => Right(bs)
        case Failure(_) => Left(InvalidParams.copy(message = s"Unable to parse data from '${input.s}'"))
      }
    } else Left(InvalidParams.copy(message = s"Data '${input.s}' should have 0x prefix"))
  }

  private def tryExtractQuantity(input: JString): Either[JsonRpcError, BigInt] = {
    if (input.s.startsWith("0x")) {
      val noPrefix = input.s.replace("0x", "")
      Try(BigInt(noPrefix, 16)) match {
        case Success(bi) => Right(bi)
        case Failure(_) => Left(InvalidParams.copy(message = s"Unable to parse quantity from '${input.s}'"))
      }
    } else Left(InvalidParams.copy(message = s"Quantity '${input.s}' should have 0x prefix"))
  }
}
