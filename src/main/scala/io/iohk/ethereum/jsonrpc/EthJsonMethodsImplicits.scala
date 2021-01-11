package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.PersonalService.{SendTransactionRequest, SendTransactionResponse, SignRequest}
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder.OptionToNull._
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec, JsonMethodDecoder}
import org.json4s.JsonAST.{JArray, JBool, JString, JValue, _}
import org.json4s.JsonDSL._
import org.json4s.{Extraction, JsonAST}

// scalastyle:off number.of.methods
object EthJsonMethodsImplicits extends JsonMethodsImplicits {
  implicit val transactionResponseJsonEncoder: JsonEncoder[TransactionResponse] = Extraction.decompose(_)

  implicit val eth_protocolVersion = new NoParamsMethodDecoder(ProtocolVersionRequest())
    with JsonEncoder[ProtocolVersionResponse] {
    def encodeJson(t: ProtocolVersionResponse): JValue = t.value
  }

  implicit val eth_chainId = new NoParamsMethodDecoder(ChainIdRequest()) with JsonEncoder[ChainIdResponse] {
    def encodeJson(t: ChainIdResponse) = encodeAsHex(t.value)
  }

  implicit val eth_blockNumber = new NoParamsMethodDecoder(BestBlockNumberRequest())
    with JsonEncoder[BestBlockNumberResponse] {
    override def encodeJson(t: BestBlockNumberResponse): JValue = Extraction.decompose(t.bestBlockNumber)
  }

  implicit val eth_submitHashrate = new JsonMethodDecoder[SubmitHashRateRequest]
    with JsonEncoder[SubmitHashRateResponse] {
    override def decodeJson(params: Option[JsonAST.JArray]): Either[JsonRpcError, SubmitHashRateRequest] =
      params match {
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

  implicit val eth_gasPrice = new NoParamsMethodDecoder(GetGasPriceRequest()) with JsonEncoder[GetGasPriceResponse] {
    override def encodeJson(t: GetGasPriceResponse): JValue = encodeAsHex(t.price)
  }

  implicit val eth_mining = new NoParamsMethodDecoder(GetMiningRequest()) with JsonEncoder[GetMiningResponse] {
    override def encodeJson(t: GetMiningResponse): JValue = JBool(t.isMining)
  }

  implicit val eth_hashrate = new NoParamsMethodDecoder(GetHashRateRequest()) with JsonEncoder[GetHashRateResponse] {
    override def encodeJson(t: GetHashRateResponse): JsonAST.JValue = encodeAsHex(t.hashRate)
  }

  implicit val eth_coinbase = new NoParamsMethodDecoder(GetCoinbaseRequest()) with JsonEncoder[GetCoinbaseResponse] {
    override def encodeJson(t: GetCoinbaseResponse): JsonAST.JValue = {
      encodeAsHex(t.address.bytes)
    }
  }

  implicit val eth_getWork = new NoParamsMethodDecoder(GetWorkRequest()) with JsonEncoder[GetWorkResponse] {
    override def encodeJson(t: GetWorkResponse): JsonAST.JValue = {
      val powHeaderHash = encodeAsHex(t.powHeaderHash)
      val dagSeed = encodeAsHex(t.dagSeed)
      val target = encodeAsHex(t.target)
      JArray(List(powHeaderHash, dagSeed, target))
    }
  }

  implicit val eth_submitWork = new JsonMethodDecoder[SubmitWorkRequest] with JsonEncoder[SubmitWorkResponse] {
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

  implicit val eth_getBlockTransactionCountByHash = new JsonMethodDecoder[TxCountByBlockHashRequest]
    with JsonEncoder[TxCountByBlockHashResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, TxCountByBlockHashRequest] =
      params match {
        case Some(JArray(JString(input) :: Nil)) =>
          extractHash(input).map(TxCountByBlockHashRequest)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: TxCountByBlockHashResponse): JValue =
      Extraction.decompose(t.txsQuantity.map(BigInt(_)))
  }

  implicit val eth_getBlockByHash = new JsonMethodDecoder[BlockByBlockHashRequest]
    with JsonEncoder[BlockByBlockHashResponse] {
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

  implicit val eth_getBlockByNumber = new JsonMethodDecoder[BlockByNumberRequest]
    with JsonEncoder[BlockByNumberResponse] {
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

  implicit val eth_pendingTransactions = new NoParamsMethodDecoder(EthPendingTransactionsRequest())
    with JsonEncoder[EthPendingTransactionsResponse] {

    override def encodeJson(t: EthPendingTransactionsResponse): JValue =
      JArray(t.pendingTransactions.toList.map { pendingTx =>
        encodeAsHex(pendingTx.stx.tx.hash)
      })
  }

  implicit val eth_getTransactionByHash =
    new JsonMethodDecoder[GetTransactionByHashRequest] with JsonEncoder[GetTransactionByHashResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByHashRequest] =
        params match {
          case Some(JArray(JString(txHash) :: Nil)) =>
            for {
              parsedTxHash <- extractHash(txHash)
            } yield GetTransactionByHashRequest(parsedTxHash)
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: GetTransactionByHashResponse): JValue =
        JsonEncoder.encode(t.txResponse)
    }

  implicit val eth_getTransactionReceipt =
    new JsonMethodDecoder[GetTransactionReceiptRequest] with JsonEncoder[GetTransactionReceiptResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionReceiptRequest] =
        params match {
          case Some(JArray(JString(txHash) :: Nil)) =>
            for {
              parsedTxHash <- extractHash(txHash)
            } yield GetTransactionReceiptRequest(parsedTxHash)
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: GetTransactionReceiptResponse): JValue =
        Extraction.decompose(t.txResponse)
    }

  implicit val GetTransactionByBlockHashAndIndexResponseEncoder =
    new JsonEncoder[GetTransactionByBlockHashAndIndexResponse] {
      override def encodeJson(t: GetTransactionByBlockHashAndIndexResponse): JValue =
        JsonEncoder.encode(t.transactionResponse)
    }

  implicit val GetTransactionByBlockHashAndIndexRequestDecoder =
    new JsonMethodDecoder[GetTransactionByBlockHashAndIndexRequest] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionByBlockHashAndIndexRequest] =
        params match {
          case Some(JArray(JString(blockHash) :: transactionIndex :: Nil)) =>
            for {
              parsedBlockHash <- extractHash(blockHash)
              parsedTransactionIndex <- extractQuantity(transactionIndex)
            } yield GetTransactionByBlockHashAndIndexRequest(parsedBlockHash, parsedTransactionIndex)
          case _ => Left(InvalidParams())
        }
    }

  implicit val GetTransactionByBlockNumberAndIndexResponseEncoder =
    new JsonEncoder[GetTransactionByBlockNumberAndIndexResponse] {
      override def encodeJson(t: GetTransactionByBlockNumberAndIndexResponse): JValue =
        JsonEncoder.encode(t.transactionResponse)
    }

  implicit val GetTransactionByBlockNumberAndIndexRequestDecoder =
    new JsonMethodDecoder[GetTransactionByBlockNumberAndIndexRequest] {
      override def decodeJson(
          params: Option[JArray]
      ): Either[JsonRpcError, GetTransactionByBlockNumberAndIndexRequest] =
        params match {
          case Some(JArray(blockParam :: transactionIndex :: Nil)) =>
            for {
              blockParam <- extractBlockParam(blockParam)
              parsedTransactionIndex <- extractQuantity(transactionIndex)
            } yield GetTransactionByBlockNumberAndIndexRequest(blockParam, parsedTransactionIndex)
          case _ => Left(InvalidParams())
        }
    }

  implicit val RawTransactionResponseJsonEncoder: JsonEncoder[RawTransactionResponse] =
    new JsonEncoder[RawTransactionResponse] {
      override def encodeJson(t: RawTransactionResponse): JValue =
        t.transactionResponse.map(RawTransactionCodec.asRawTransaction _ andThen encodeAsHex)
    }

  implicit val eth_getUncleByBlockHashAndIndex = new JsonMethodDecoder[UncleByBlockHashAndIndexRequest]
    with JsonEncoder[UncleByBlockHashAndIndexResponse] {
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
      uncleBlockResponse.removeField {
        case JField("transactions", _) => true
        case _ => false
      }
    }
  }

  implicit val eth_getUncleByBlockNumberAndIndex = new JsonMethodDecoder[UncleByBlockNumberAndIndexRequest]
    with JsonEncoder[UncleByBlockNumberAndIndexResponse] {
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
      uncleBlockResponse.removeField {
        case JField("transactions", _) => true
        case _ => false
      }
    }
  }

  implicit val eth_syncing = new NoParamsMethodDecoder(SyncingRequest()) with JsonEncoder[SyncingResponse] {
    def encodeJson(t: SyncingResponse): JValue = t.syncStatus match {
      case Some(syncStatus) => Extraction.decompose(syncStatus)
      case None => false
    }
  }

  implicit val eth_sendRawTransaction = new JsonMethodDecoder[SendRawTransactionRequest]
    with JsonEncoder[SendRawTransactionResponse] {
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

  implicit val eth_sendTransaction = new JsonMethodCodec[SendTransactionRequest, SendTransactionResponse] {
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

  implicit val eth_call = new JsonMethodDecoder[CallRequest] with JsonEncoder[CallResponse] {
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
  }

  implicit val eth_estimateGas = new JsonMethodDecoder[CallRequest] with JsonEncoder[EstimateGasResponse] {
    override def encodeJson(t: EstimateGasResponse): JValue = encodeAsHex(t.gas)

    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, CallRequest] =
      withoutBlockParam.applyOrElse(params, eth_call.decodeJson)

    def withoutBlockParam: PartialFunction[Option[JArray], Either[JsonRpcError, CallRequest]] = {
      case Some(JArray((txObj: JObject) :: Nil)) =>
        extractCall(txObj).map(CallRequest(_, BlockParam.Latest))
    }

  }

  implicit val eth_getCode = new JsonMethodDecoder[GetCodeRequest] with JsonEncoder[GetCodeResponse] {
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

  implicit val eth_getUncleCountByBlockNumber = new JsonMethodDecoder[GetUncleCountByBlockNumberRequest]
    with JsonEncoder[GetUncleCountByBlockNumberResponse] {
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

  implicit val eth_getUncleCountByBlockHash = new JsonMethodDecoder[GetUncleCountByBlockHashRequest]
    with JsonEncoder[GetUncleCountByBlockHashResponse] {
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

  implicit val eth_getBlockTransactionCountByNumber = new JsonMethodDecoder[GetBlockTransactionCountByNumberRequest]
    with JsonEncoder[GetBlockTransactionCountByNumberResponse] {
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

  implicit val eth_getBalance = new JsonMethodDecoder[GetBalanceRequest] with JsonEncoder[GetBalanceResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetBalanceRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetBalanceRequest(address, block)
        case other =>
          Left(InvalidParams())
      }

    def encodeJson(t: GetBalanceResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getStorageAt = new JsonMethodDecoder[GetStorageAtRequest] with JsonEncoder[GetStorageAtResponse] {
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

  implicit val eth_getTransactionCount = new JsonMethodDecoder[GetTransactionCountRequest]
    with JsonEncoder[GetTransactionCountResponse] {
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

  implicit val eth_newFilter = new JsonMethodDecoder[NewFilterRequest] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, NewFilterRequest] =
      params match {
        case Some(JArray((filterObj: JObject) :: Nil)) =>
          for {
            filter <- extractFilter(filterObj)
          } yield NewFilterRequest(filter)
        case _ => Left(InvalidParams())
      }
  }

  implicit val eth_newBlockFilter = new NoParamsMethodDecoder(NewBlockFilterRequest()) {}

  implicit val eth_newPendingTransactionFilter = new NoParamsMethodDecoder(NewPendingTransactionFilterRequest()) {}

  implicit val eth_uninstallFilter = new JsonMethodDecoder[UninstallFilterRequest]
    with JsonEncoder[UninstallFilterResponse] {
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

  implicit val eth_getFilterChanges = new JsonMethodDecoder[GetFilterChangesRequest]
    with JsonEncoder[GetFilterChangesResponse] {
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
        case FilterManager.LogFilterChanges(logs) => JArray(logs.map(Extraction.decompose).toList)
        case FilterManager.BlockFilterChanges(blockHashes) => JArray(blockHashes.map(encodeAsHex).toList)
        case FilterManager.PendingTransactionFilterChanges(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

  implicit val eth_getFilterLogs = new JsonMethodDecoder[GetFilterLogsRequest] with JsonEncoder[GetFilterLogsResponse] {
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
        case BlockFilterLogs(blockHashes) => JArray(blockHashes.map(encodeAsHex).toList)
        case PendingTransactionFilterLogs(txHashes) => JArray(txHashes.map(encodeAsHex).toList)
      }
  }

  implicit val eth_getLogs = new JsonMethodDecoder[GetLogsRequest] with JsonEncoder[GetLogsResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetLogsRequest] =
      params match {
        case Some(JArray((filterObj: JObject) :: Nil)) =>
          for {
            filter <- extractFilter(filterObj)
          } yield GetLogsRequest(filter)
        case _ => Left(InvalidParams())
      }

    override def encodeJson(t: GetLogsResponse): JValue =
      JArray(t.filterLogs.logs.map(Extraction.decompose).toList)
  }

  private def extractFilter(obj: JObject): Either[JsonRpcError, Filter] = {
    def allSuccess[T](eithers: Seq[Either[JsonRpcError, T]]): Either[JsonRpcError, Seq[T]] = {
      if (eithers.forall(_.isRight)) {
        val values = eithers.collect { case Right(v) => v }
        Right(values)
      } else {
        val values = eithers.collect { case Left(err) => err.message }
        Left(InvalidParams(msg = values.mkString("\n")))
      }
    }

    def parseTopic(jstr: JString): Either[JsonRpcError, ByteString] = {
      extractBytes(jstr).left.map(_ =>
        InvalidParams(msg = s"Unable to parse topics, expected byte data but got ${jstr.values}")
      )
    }

    def parseNestedTopics(jarr: JArray): Either[JsonRpcError, Seq[ByteString]] = {
      allSuccess(jarr.arr.map {
        case jstr: JString => parseTopic(jstr)
        case other => Left(InvalidParams(msg = s"Unable to parse topics, expected byte data but got: $other"))
      })
    }

    val topicsEither: Either[JsonRpcError, Seq[Seq[ByteString]]] =
      allSuccess((obj \ "topics").extractOpt[JArray].map(_.arr).getOrElse(Nil).map {
        case JNull => Right(Nil)
        case jstr: JString => parseTopic(jstr).map(Seq(_))
        case jarr: JArray => parseNestedTopics(jarr)
        case other => Left(InvalidParams(msg = s"Unable to parse topics, expected byte data or array but got: $other"))
      })

    def optionalBlockParam(field: String) =
      (obj \ field).extractOpt[JValue].flatMap {
        case JNothing => None
        case other => Some(extractBlockParam(other))
      }

    for {
      fromBlock <- toEitherOpt(optionalBlockParam("fromBlock"))
      toBlock <- toEitherOpt(optionalBlockParam("toBlock"))
      address <- toEitherOpt((obj \ "address").extractOpt[String].map(extractAddress))
      topics <- topicsEither
    } yield Filter(fromBlock = fromBlock, toBlock = toBlock, address = address, topics = topics)
  }

  implicit val eth_sign = new JsonMethodDecoder[SignRequest] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, SignRequest] =
      params match {
        case Some(JArray(JString(addr) :: JString(message) :: _)) =>
          for {
            message <- extractBytes(message)
            address <- extractAddress(addr)
          } yield SignRequest(message, address, None)
        case _ =>
          Left(InvalidParams())
      }
  }

  def extractCall(obj: JObject): Either[JsonRpcError, CallTx] = {
    def toEitherOpt[A, B](opt: Option[Either[A, B]]): Either[A, Option[B]] = {
      opt match {
        case Some(Right(v)) => Right(Option(v))
        case Some(Left(e)) => Left(e)
        case None => Right(None)
      }
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
      gas = gas,
      gasPrice = gasPrice.getOrElse(0),
      value = value.getOrElse(0),
      data = data.getOrElse(ByteString(""))
    )
  }

  implicit val eth_getStorageRoot = new JsonMethodDecoder[GetStorageRootRequest]
    with JsonEncoder[GetStorageRootResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetStorageRootRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetStorageRootRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetStorageRootResponse): JValue = encodeAsHex(t.storageRoot)
  }
}
