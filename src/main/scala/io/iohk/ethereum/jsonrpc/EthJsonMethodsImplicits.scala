package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.FilterManager.{Filter => _, NewFilterResponse => _, UninstallFilterResponse => _, _}
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.jsonrpc.PersonalService.{SendTransactionRequest, SendTransactionResponse}
import org.json4s.{Extraction, JsonAST}
import org.json4s.JsonAST.{JArray, JBool, JString, JValue, _}
import org.json4s.JsonDSL._

import scala.util.{Failure, Success, Try}

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
        case Some(JArray(JString(blockHash) :: JBool(txHashed) :: Nil)) =>
          extractHash(blockHash).map(BlockByBlockHashRequest(_, txHashed))
        case _ => Left(InvalidParams())
      }
    }

    override def encodeJson(t: BlockByBlockHashResponse): JValue =
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
        case Some(JArray((txObj: JObject) :: (blockValue: JValue) :: Nil)) =>
          for {
            blockParam <- extractBlockParam(blockValue)
            tx <- extractCall(txObj)
          } yield CallRequest(tx, blockParam)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: CallResponse): JValue = encodeAsHex(t.returnData)

    def extractCall(obj: JObject): Either[JsonRpcError, CallTx] = {
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
        case Some(JArray((address: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            addr <- extractAddress(address)
            block <- extractBlockParam(blockValue)
          } yield GetCodeRequest(addr, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetCodeResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getUncleCountByBlockNumber = new JsonDecoder[GetUncleCountByBlockNumberRequest] with JsonEncoder[GetUncleCountByBlockNumberResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetUncleCountByBlockNumberRequest] =
      params match {
        case Some(JArray((blockValue: JValue) :: Nil)) =>
          for {
            block <- extractBlockParam(blockValue)
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
        case Some(JArray((blockValue: JValue) :: Nil)) =>
          for {
            block <- extractBlockParam(blockValue)
          } yield GetBlockTransactionCountByNumberRequest(block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetBlockTransactionCountByNumberResponse): JValue = encodeAsHex(t.result)
  }

  implicit val eth_getBalance = new JsonDecoder[GetBalanceRequest] with JsonEncoder[GetBalanceResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetBalanceRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetBalanceRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetBalanceResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getStorageAt = new JsonDecoder[GetStorageAtRequest] with JsonEncoder[GetStorageAtResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetStorageAtRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (positionStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            position <- extractQuantity(positionStr)
            block <- extractBlockParam(blockValue)
          } yield GetStorageAtRequest(address, position, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetStorageAtResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getTransactionCount = new JsonDecoder[GetTransactionCountRequest] with JsonEncoder[GetTransactionCountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionCountRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetTransactionCountRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetTransactionCountResponse): JValue = encodeAsHex(t.value)
  }

  implicit val newFilterResponseEnc = new JsonEncoder[NewFilterResponse] {
    def encodeJson(t: NewFilterResponse): JValue = encodeAsHex(t.filterId)
  }

  implicit val eth_newFilter = new JsonDecoder[NewFilterRequest] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewFilterRequest] =
      params match {
        case Some(JArray((filterObj: JObject) :: Nil)) =>
          for {
            filter <- extractFilter(filterObj)
          } yield NewFilterRequest(filter)
        case _ => Left(InvalidParams())
      }

    def extractFilter(obj: JObject): Either[JsonRpcError, Filter] = {
      val topicsEither: Either[JsonRpcError, Seq[Seq[ByteString]]] =
        Try((obj \ "topics").extractOpt[JArray].map(_.arr).getOrElse(Nil).map {
          case JNull => Nil
          case js: JString =>
            Seq(extractBytes(js).toOption.getOrElse(throw new RuntimeException(s"Unable to parse topics, expected byte data but got  ${js.values}")))
          case jarr: JArray => jarr.arr.map {
            case e: JString =>
              extractBytes(e).toOption.getOrElse(throw new RuntimeException(s"Unable to parse topics, expected byte data but got ${e.values}"))
            case other =>
              throw new RuntimeException(s"Unable to parse topics, expected byte data but got: $other")
          }
          case other =>
            throw new RuntimeException(s"Unable to parse topics, expected byte data or array but got: $other")
        }) match {
          case Success(topics) => Right(topics)
          case Failure(ex) => Left(JsonRpcErrors.InvalidParams(ex.getMessage))
        }

      for {
        fromBlock <- toEitherOpt((obj \ "fromBlock").extractOpt[JValue].map(extractBlockParam))
        toBlock <- toEitherOpt((obj \ "toBlock").extractOpt[JValue].map(extractBlockParam))
        address <- toEitherOpt((obj \ "address").extractOpt[String].map(extractAddress))
        topics <- topicsEither
      } yield Filter(
        fromBlock = fromBlock,
        toBlock = toBlock,
        address = address,
        topics = topics)
    }
  }

  implicit val eth_newBlockFilter = new JsonDecoder[NewBlockFilterRequest] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewBlockFilterRequest] =
      Right(NewBlockFilterRequest())
  }

  implicit val eth_newPendingTransactionFilter = new JsonDecoder[NewPendingTransactionFilterRequest] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewPendingTransactionFilterRequest] =
      Right(NewPendingTransactionFilterRequest())
  }

  implicit val eth_uninstallFilter = new JsonDecoder[UninstallFilterRequest] with JsonEncoder[UninstallFilterResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, UninstallFilterRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield UninstallFilterRequest(filterId)
        case _ => Left(InvalidParams())
      }
    override def encodeJson(t: UninstallFilterResponse): JValue = JBool(t.success)
  }

  implicit val eth_getFilterChanges = new JsonDecoder[GetFilterChangesRequest] with JsonEncoder[GetFilterChangesResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetFilterChangesRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield GetFilterChangesRequest(filterId)
        case _ => Left(InvalidParams())
      }
    override def encodeJson(t: GetFilterChangesResponse): JValue =
      t.filterChanges match {
        case LogFilterChanges(logs) => JArray(logs.map(Extraction.decompose).toList)
        case BlockFilterChanges(blockHashes) =>  JArray(blockHashes.map(encodeAsHex).toList)
        case PendingTransactionFilterChanges(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

  implicit val eth_getFilterLogs = new JsonDecoder[GetFilterLogsRequest] with JsonEncoder[GetFilterLogsResponse] {
    import FilterManager._

    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetFilterLogsRequest] =
      params match {
        case Some(JArray((rawFilterId: JValue) :: Nil)) =>
          for {
            filterId <- extractQuantity(rawFilterId)
          } yield GetFilterLogsRequest(filterId)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: GetFilterLogsResponse): JValue =
      t.filterLogs match {
        case LogFilterLogs(logs) => JArray(logs.map(Extraction.decompose).toList)
        case BlockFilterLogs(blockHashes) =>  JArray(blockHashes.map(encodeAsHex).toList)
        case PendingTransactionFilterLogs(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

}
