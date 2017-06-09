package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.jsonrpc.PersonalService.{SendTransactionRequest, SendTransactionResponse}
import org.json4s.{Extraction, JsonAST}
import org.json4s.JsonAST.{JArray, JBool, JString, JValue, _}
import org.json4s.JsonDSL._

// scalastyle:off number.of.methods
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
      case Some(JArray(hashRate :: JString(id) :: Nil)) =>
        val result: Either[JsonRpcError, SubmitHashRateRequest] = for {
          rate <- extractQuantity(hashRate)
          miner <- extractHash(id)
        } yield SubmitHashRateRequest(rate, miner)
        result
      case _ =>
        Left(InvalidParams())
    }

    override def encodeJson(t: SubmitHashRateResponse): JValue = JBool(t.success)
  }

  implicit val eth_mining = new JsonDecoder[GetMiningRequest] with JsonEncoder[GetMiningResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetMiningRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetMiningRequest())
      case Some(_) => Left(InvalidParams())
    }

    override def encodeJson(t: GetMiningResponse): JValue = JBool(t.isMining)
  }

  implicit val eth_hashrate = new JsonDecoder[GetHashRateRequest] with JsonEncoder[GetHashRateResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetHashRateRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetHashRateRequest())
      case Some(_) => Left(InvalidParams())
    }

    override def encodeJson(t: GetHashRateResponse): JsonAST.JValue = encodeAsHex(t.hashRate)
  }

  implicit val eth_coinbase = new JsonDecoder[GetCoinbaseRequest] with JsonEncoder[GetCoinbaseResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetCoinbaseRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetCoinbaseRequest())
      case Some(_) => Left(InvalidParams())
    }

    override def encodeJson(t: GetCoinbaseResponse): JsonAST.JValue ={
      encodeAsHex(t.address.bytes)
    }
  }

  implicit val eth_getWork = new JsonDecoder[GetWorkRequest] with JsonEncoder[GetWorkResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetWorkRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetWorkRequest())
      case Some(_) => Left(InvalidParams())
    }

    override def encodeJson(t: GetWorkResponse): JsonAST.JValue = {
      val powHeaderHash = encodeAsHex(t.powHeaderHash)
      val dagSeed = encodeAsHex(t.dagSeed)
      val target = encodeAsHex(t.target)
      JArray(List(powHeaderHash, dagSeed, target))
    }
  }

  implicit val eth_submitWork = new JsonDecoder[SubmitWorkRequest] with JsonEncoder[SubmitWorkResponse] {
    override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitWorkRequest] = params match {
      case Some(JArray(JString(nonce) :: JString(powHeaderHash) :: JString(mixHash) :: Nil)) =>
        for {
          n <- extractBytes(nonce)
          p <- extractBytes(powHeaderHash)
          m <- extractBytes(mixHash)
        } yield SubmitWorkRequest(n, p, m)
      case _ =>
        Left(InvalidParams())
    }

    override def encodeJson(t: SubmitWorkResponse): JValue = JBool(t.success)
  }

  implicit val eth_getBlockTransactionCountByHash = new JsonDecoder[TxCountByBlockHashRequest] with JsonEncoder[TxCountByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, TxCountByBlockHashRequest] =
      params match {
        case Some(JArray(JString(input) :: Nil)) =>
          extractHash(input).map(TxCountByBlockHashRequest)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: TxCountByBlockHashResponse): JValue =
      Extraction.decompose(t.txsQuantity.map(BigInt(_)))
  }

  implicit val eth_getBlockByHash = new JsonDecoder[BlockByBlockHashRequest] with JsonEncoder[BlockByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BlockByBlockHashRequest] = {
      params match {
        case Some(JArray(JString(blockHash) :: JBool(fullTxs) :: Nil)) =>
          extractHash(blockHash).map(BlockByBlockHashRequest(_, fullTxs))
        case _ => Left(InvalidParams())
      }
    }

    override def encodeJson(t: BlockByBlockHashResponse): JValue =
      Extraction.decompose(t.blockResponse)
  }

  implicit val eth_getBlockByNumber = new JsonDecoder[BlockByNumberRequest] with JsonEncoder[BlockByNumberResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, BlockByNumberRequest] = {
      params match {
        case Some(JArray(blockStr :: JBool(fullTxs) :: Nil)) =>
          extractBlockParam(blockStr).map(BlockByNumberRequest(_, fullTxs))
        case _ => Left(InvalidParams())
      }
    }

    override def encodeJson(t: BlockByNumberResponse): JValue =
      Extraction.decompose(t.blockResponse)
  }

  implicit val eth_getTransactionByBlockHashAndIndex =
    new JsonDecoder[GetTransactionByBlockHashAndIndexRequest] with JsonEncoder[GetTransactionByBlockHashAndIndexResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByBlockHashAndIndexRequest] = params match {
        case Some(JArray(JString(blockHash) :: transactionIndex :: Nil)) =>
          for {
            parsedBlockHash <- extractHash(blockHash)
            parsedTransactionIndex <- extractQuantity(transactionIndex)
          } yield GetTransactionByBlockHashAndIndexRequest(parsedBlockHash, parsedTransactionIndex)
        case _ => Left(InvalidParams())
      }

      override def encodeJson(t: GetTransactionByBlockHashAndIndexResponse): JValue =
        t.transactionResponse.map(Extraction.decompose).getOrElse(JNull)
    }

  implicit val eth_getUncleByBlockHashAndIndex = new JsonDecoder[UncleByBlockHashAndIndexRequest] with JsonEncoder[UncleByBlockHashAndIndexResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, UncleByBlockHashAndIndexRequest] =
      params match {
        case Some(JArray(JString(blockHash) :: uncleIndex :: Nil)) =>
          for {
            hash <- extractHash(blockHash)
            uncleBlockIndex <- extractQuantity(uncleIndex)
          } yield UncleByBlockHashAndIndexRequest(hash, uncleBlockIndex)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: UncleByBlockHashAndIndexResponse): JValue = {
      val uncleBlockResponse = Extraction.decompose(t.uncleBlockResponse)
      uncleBlockResponse.removeField{
        case JField("transactions", _) => true
        case _ => false
      }
    }
  }

  implicit val eth_getUncleByBlockNumberAndIndex = new JsonDecoder[UncleByBlockNumberAndIndexRequest] with JsonEncoder[UncleByBlockNumberAndIndexResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, UncleByBlockNumberAndIndexRequest] =
      params match {
        case Some(JArray(blockStr :: uncleIndex :: Nil)) =>
          for {
            block <- extractBlockParam(blockStr)
            uncleBlockIndex <- extractQuantity(uncleIndex)
          } yield UncleByBlockNumberAndIndexRequest(block, uncleBlockIndex)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: UncleByBlockNumberAndIndexResponse): JValue = {
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

  implicit val eth_sendRawTransaction = new JsonDecoder[SendRawTransactionRequest] with JsonEncoder[SendRawTransactionResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendRawTransactionRequest] =
      params match {
        case Some(JArray(JString(dataStr) :: Nil)) =>
          for {
            data <- extractBytes(dataStr)
          } yield SendRawTransactionRequest(data)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: SendRawTransactionResponse): JValue = encodeAsHex(t.transactionHash)
  }

  implicit val eth_sendTransaction = new Codec[SendTransactionRequest, SendTransactionResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendTransactionRequest] =
      params match {
        case Some(JArray(JObject(tx) :: _)) =>
          extractTx(tx.toMap).map(SendTransactionRequest)
        case _ =>
          Left(InvalidParams())
      }

    def encodeJson(t: SendTransactionResponse): JValue =
      encodeAsHex(t.txHash)
  }

  implicit val eth_call = new JsonDecoder[CallRequest] with JsonEncoder[CallResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, CallRequest] =
      params match {
        case Some(JArray((txObj: JObject) :: (blockStr: JString) :: Nil)) =>
          for {
            blockParam <- extractBlockParam(blockStr)
            tx <- extractCall(txObj)
          } yield CallRequest(tx, blockParam)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: CallResponse): JValue = encodeAsHex(t.returnData)

    def extractCall(obj: JObject): Either[JsonRpcError, CallTx] = {
      def toEitherOpt[A, B](opt: Option[Either[A, B]]): Either[A, Option[B]] =
        opt.map(_.right.map(Some.apply)).getOrElse(Right(None))

      def optionalQuantity(input: JValue): Either[JsonRpcError, Option[BigInt]] =
        input match {
          case JNothing => Right(None)
          case o => extractQuantity(o).map(Some(_))
        }

      for {
        from <- toEitherOpt((obj \ "from").extractOpt[String].map(extractBytes))
        to <- toEitherOpt((obj \ "to").extractOpt[String].map(extractBytes))
        gas <- optionalQuantity(obj \ "gas")
        gasPrice <- optionalQuantity(obj \ "gasPrice")
        value <- optionalQuantity(obj \ "value")
        data <- toEitherOpt((obj \ "data").extractOpt[String].map(extractBytes))
      } yield CallTx(
        from = from,
        to = to,
        gas = gas.getOrElse(0),
        gasPrice = gasPrice.getOrElse(0),
        value = value.getOrElse(0),
        data = data.getOrElse(ByteString("")))
    }
  }

  implicit val eth_getCode = new JsonDecoder[GetCodeRequest] with JsonEncoder[GetCodeResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetCodeRequest] =
      params match {
        case Some(JArray((address: JString) :: (blockStr: JString) :: Nil)) =>
          for {
            addr <- extractAddress(address)
            block <- extractBlockParam(blockStr)
          } yield GetCodeRequest(addr, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetCodeResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getUncleCountByBlockNumber = new JsonDecoder[GetUncleCountByBlockNumberRequest] with JsonEncoder[GetUncleCountByBlockNumberResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetUncleCountByBlockNumberRequest] =
      params match {
        case Some(JArray((blockStr: JString) :: Nil)) =>
          for {
            block <- extractBlockParam(blockStr)
          } yield GetUncleCountByBlockNumberRequest(block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetUncleCountByBlockNumberResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getUncleCountByBlockHash = new JsonDecoder[GetUncleCountByBlockHashRequest] with JsonEncoder[GetUncleCountByBlockHashResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetUncleCountByBlockHashRequest] =
      params match {
        case Some(JArray(JString(hash) :: Nil)) =>
          for {
            blockHash <- extractHash(hash)
          } yield GetUncleCountByBlockHashRequest(blockHash)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetUncleCountByBlockHashResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getBlockTransactionCountByNumber = new JsonDecoder[GetBlockTransactionCountByNumberRequest] with
    JsonEncoder[GetBlockTransactionCountByNumberResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetBlockTransactionCountByNumberRequest] =
      params match {
        case Some(JArray((blockStr: JString) :: Nil)) =>
          for {
            block <- extractBlockParam(blockStr)
          } yield GetBlockTransactionCountByNumberRequest(block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetBlockTransactionCountByNumberResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getBalance = new JsonDecoder[GetBalanceRequest] with JsonEncoder[GetBalanceResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetBalanceRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockStr: JString) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockStr)
          } yield GetBalanceRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetBalanceResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getStorageAt = new JsonDecoder[GetStorageAtRequest] with JsonEncoder[GetStorageAtResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetStorageAtRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (positionStr: JString) :: (blockStr: JString) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            position <- extractQuantity(positionStr)
            block <- extractBlockParam(blockStr)
          } yield GetStorageAtRequest(address, position, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetStorageAtResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getTransactionCount = new JsonDecoder[GetTransactionCountRequest] with JsonEncoder[GetTransactionCountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionCountRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockStr: JString) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockStr)
          } yield GetTransactionCountRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetTransactionCountResponse): JValue = encodeAsHex(t.value)
  }

}
