package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.ProofService.{GetProofRequest, GetProofResponse, StorageProofKey}
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder
import org.json4s.JsonAST.{JArray, JString, JValue, _}

object EthProofJsonMethodsImplicits extends JsonMethodsImplicits {
  def extractStorageKeys(input: JValue): Either[JsonRpcError, Seq[StorageProofKey]] = {
    import cats.syntax.traverse._
    import cats.syntax.either._
    input match {
      case JArray(elems) =>
        elems.traverse { x =>
          extractQuantity(x)
            .map(StorageProofKey.apply)
            .leftMap(_ => InvalidParams(s"Invalid param storage proof key: $x"))
        }
      case _ => Left(InvalidParams())
    }
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
