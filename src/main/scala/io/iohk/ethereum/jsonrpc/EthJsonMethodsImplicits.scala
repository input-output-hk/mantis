package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.EthService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.PersonalService.{SendTransactionRequest, SendTransactionResponse, SignRequest}
import io.iohk.ethereum.jsonrpc.ProofService.{GetProofRequest, GetProofResponse, StorageProofKey}
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder.OptionToNull._
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec, JsonMethodDecoder}
import org.json4s.JsonAST.{JArray, JString, JValue, _}
import org.json4s.JsonDSL._
import org.json4s.Extraction

object EthJsonMethodsImplicits extends JsonMethodsImplicits {
//  implicit val transactionResponseJsonEncoder: JsonEncoder[TransactionResponse] = Extraction.decompose(_)

  implicit val eth_protocolVersion = new NoParamsMethodDecoder(ProtocolVersionRequest())
    with JsonEncoder[ProtocolVersionResponse] {
    def encodeJson(t: ProtocolVersionResponse): JValue = t.value
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
