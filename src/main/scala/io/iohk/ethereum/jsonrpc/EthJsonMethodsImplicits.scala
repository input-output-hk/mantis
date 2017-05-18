package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import org.json4s.{Extraction, JsonAST}
import org.json4s.JsonAST.{JArray, JBool, JString, JValue, _}
import org.json4s.JsonDSL._

object EthJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val eth_protocolVersion = new JsonDecoder[ProtocolVersionRequest] with JsonEncoder[ProtocolVersionResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, ProtocolVersionRequest] = Right(ProtocolVersionRequest())

    def encodeJson(t: ProtocolVersionResponse): JValue = t.value
  }

  implicit val eth_blockNumber = new JsonDecoder[BestBlockNumberRequest] with JsonEncoder[BestBlockNumberResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BestBlockNumberRequest] = Right(BestBlockNumberRequest())

    override def encodeJson(t: BestBlockNumberResponse): JValue = Extraction.decompose(t.bestBlockNumber)
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
      case None | Some(JArray(Nil)) => Right(GetWorkRequest())
      case Some(_) => Left(InvalidParams)
    }

    override def encodeJson(t: GetWorkResponse): JsonAST.JValue ={
      val powHeaderHash = encodeAsHex(t.powHeaderHash)
      val dagSeed = encodeAsHex(t.dagSeed)
      val target = encodeAsHex(t.target)
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

  implicit val eth_getBlockTransactionCountByHash = new JsonDecoder[TxCountByBlockHashRequest] with JsonEncoder[TxCountByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, TxCountByBlockHashRequest] =
      params match {
        case Some(JArray((input: JString) :: Nil)) =>
          tryExtractUnformattedData(input).map(TxCountByBlockHashRequest)
        case _ => Left(InvalidParams)
      }

    override def encodeJson(t: TxCountByBlockHashResponse): JValue =
      Extraction.decompose(t.txsQuantity.map(BigInt(_)))
  }

  implicit val eth_getBlockByHash = new JsonDecoder[BlockByBlockHashRequest] with JsonEncoder[BlockByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BlockByBlockHashRequest] = {
      params match {
        case Some(JArray((blockHash: JString) :: JBool(txHashed) :: Nil)) =>
          tryExtractUnformattedData(blockHash).map(BlockByBlockHashRequest(_, txHashed))
        case _ => Left(InvalidParams)
      }
    }

    override def encodeJson(t: BlockByBlockHashResponse): JValue =
      Extraction.decompose(t.blockResponse)
  }

  implicit val eth_getTransactionByBlockHashAndIndex =
    new JsonDecoder[GetTransactionByBlockHashAndIndexRequest] with JsonEncoder[GetTransactionByBlockHashAndIndexResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByBlockHashAndIndexRequest] = params match {
        case Some(JArray((blockHash: JString) :: (transactionIndex: JString) :: Nil)) =>
          for {
            parsedBlockHash <- tryExtractUnformattedData(blockHash)
            parsedTransactionIndex <- tryExtractQuantity(transactionIndex)
          } yield GetTransactionByBlockHashAndIndexRequest(parsedBlockHash, parsedTransactionIndex)
        case _ => Left(InvalidParams)
      }

      override def encodeJson(t: GetTransactionByBlockHashAndIndexResponse): JValue =
        t.transactionResponse.map(Extraction.decompose).getOrElse(JNull)
    }

  implicit val eth_getUncleByBlockHashAndIndex = new JsonDecoder[UncleByBlockHashAndIndexRequest] with JsonEncoder[UncleByBlockHashAndIndexResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, UncleByBlockHashAndIndexRequest] =
      params match {
        case Some(JArray((blockHash: JString) :: (uncleIndex: JString) :: Nil)) =>
          for {
            hash <- tryExtractUnformattedData(blockHash)
            uncleBlockIndex <- tryExtractQuantity(uncleIndex)
          } yield UncleByBlockHashAndIndexRequest(hash, uncleBlockIndex)
        case _ => Left(InvalidParams)
      }

    override def encodeJson(t: UncleByBlockHashAndIndexResponse): JValue = {
      val uncleBlockResponse = Extraction.decompose(t.uncleBlockResponse)
      uncleBlockResponse.removeField{
        case JField("transactions", _) => true
        case _ => false
      }
    }
  }

  implicit val eth_syncing = new JsonDecoder[SyncingRequest] with JsonEncoder[SyncingResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SyncingRequest] = Right(SyncingRequest())

    def encodeJson(t: SyncingResponse): JValue = Extraction.decompose(t)
  }
}
