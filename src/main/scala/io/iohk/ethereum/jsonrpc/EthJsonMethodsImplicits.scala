package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthBlocksService._
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.EthMiningService._
import io.iohk.ethereum.jsonrpc.PersonalService.{SendTransactionRequest, SendTransactionResponse, SignRequest}
import io.iohk.ethereum.jsonrpc.ProofService.{GetProofRequest, GetProofResponse, StorageProofKey}
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

  implicit val eth_getProof: JsonMethodDecoder[GetProofRequest] with JsonEncoder[GetProofResponse] =
    new JsonMethodDecoder[GetProofRequest] with JsonEncoder[GetProofResponse] {
      override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetProofRequest] =
        params match {
          case Some(JArray((address: JString) :: storageKeys :: (blockNumber: JValue) :: Nil)) =>
            for {
              addressParsed <- extractAddress(address)
              storageKeysParsed <- extractStorageKeys(storageKeys)
              blockNumberParsed <- extractBlockParam(blockNumber)
            } yield GetProofRequest(addressParsed, storageKeysParsed, blockNumberParsed)
          case _ => Left(InvalidParams())
        }

      override def encodeJson(t: GetProofResponse): JValue = {
        JObject(
          "accountProof" -> JArray(t.proofAccount.accountProof.toList.map { ap => encodeAsHex(ap) }),
          "balance" -> encodeAsHex(t.proofAccount.balance),
          "codeHash" -> encodeAsHex(t.proofAccount.codeHash),
          "nonce" -> encodeAsHex(t.proofAccount.nonce),
          "storageHash" -> encodeAsHex(t.proofAccount.storageHash),
          "storageProof" -> JArray(t.proofAccount.storageProof.toList.map { sp =>
            JObject(
              "key" -> encodeAsHex(sp.key.v),
              "proof" -> JArray(sp.proof.toList.map { p => encodeAsHex(p) }),
              "value" -> encodeAsHex(sp.value)
            )
          })
        )
      }
    }
}
