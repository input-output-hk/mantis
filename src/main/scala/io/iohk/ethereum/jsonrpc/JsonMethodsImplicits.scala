package io.iohk.ethereum.jsonrpc

import java.nio.ByteBuffer

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import org.json4s.{DefaultFormats, Formats, JValue}
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

object JsonMethodsImplicits {

  import JsonRpcErrors._

  implicit val formats: Formats = DefaultFormats ++ List(BlockViewSerializer)

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

  implicit val eth_getBlockTransactionCountByHash = new JsonDecoder[TxCountByBlockHashRequest] with JsonEncoder[TxCountByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, TxCountByBlockHashRequest] =
      params match {
        case Some(JArray((input: JString) :: Nil)) if input.s.startsWith("0x") =>
          Try(ByteString(Hex.decode(input.s.drop(2)))) match {
            case Success(bs) => Right(TxCountByBlockHashRequest(bs))
            case Failure(_) => Left(InvalidParams)
          }
        case _ => Left(InvalidParams)
      }

    override def encodeJson(t: TxCountByBlockHashResponse): JValue = {
      t.txsQuantity match {
        case Some(txsCount) =>
          val intAsHexNoZeroes = Hex.toHexString(ByteBuffer.allocate(Integer.BYTES).putInt(txsCount).array().dropWhile(_ == 0))
          val intAsHex = if(intAsHexNoZeroes.isEmpty) "0" else intAsHexNoZeroes
          JString(s"0x$intAsHex")
        case None => JNull
      }
    }
  }

  implicit val eth_getBlockByHash = new JsonDecoder[BlockByBlockHashRequest] with JsonEncoder[BlockByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BlockByBlockHashRequest] = {
      params match {
        case Some(JArray((blockHash: JString) :: (txHashed: JBool) :: Nil)) if blockHash.s.startsWith("0x") =>
          Try(ByteString(Hex.decode(blockHash.s.drop(2)))) match {
            case Success(bs) => Right(BlockByBlockHashRequest(bs, txHashed.value))
            case Failure(_) => Left(InvalidParams)
          }
        case _ => Left(InvalidParams)
      }
    }

    override def encodeJson(t: BlockByBlockHashResponse): JValue =
      t.blockView.map(BlockView.jsonEncode).getOrElse(JNull)
  }

  implicit val eth_getUncleByBlockHashAndIndex = new JsonDecoder[UncleByBlockHashAndIndexRequest] with JsonEncoder[UncleByBlockHashAndIndexResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, UncleByBlockHashAndIndexRequest] =
      params match {
        case Some(JArray((blockHash: JString) :: (uncleIndex: JInt) :: Nil)) if blockHash.s.startsWith("0x") =>
          Try(ByteString(Hex.decode(blockHash.s.drop(2)))) match {
            case Success(bs) => Right(UncleByBlockHashAndIndexRequest(bs, uncleIndex.num.toInt))
            case Failure(_) => Left(InvalidParams)
          }
        case _ => Left(InvalidParams)
      }

    override def encodeJson(t: UncleByBlockHashAndIndexResponse): JValue =
      t.uncleBlockView.map(BlockView.jsonEncode).getOrElse(JNull)
  }

}
