package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import org.json4s.Extraction
import org.json4s.JsonAST
import org.json4s.JsonAST._

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.jsonrpc.CheckpointingService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodCodec
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.QuantitiesSerializer

object CheckpointingJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val checkpointing_getLatestBlock: JsonMethodCodec[GetLatestBlockRequest, GetLatestBlockResponse] =
    new JsonMethodCodec[GetLatestBlockRequest, GetLatestBlockResponse] {
      override def decodeJson(
          params: Option[JsonAST.JArray]
      ): Either[JsonRpcError, GetLatestBlockRequest] =
        params match {
          case Some(JArray(JInt(chkpInterval) :: second :: Nil)) =>
            if (chkpInterval > 0 && chkpInterval <= Int.MaxValue)
              second match {
                case JString(blockHash) =>
                  for {
                    hash <- extractHash(blockHash)
                  } yield GetLatestBlockRequest(chkpInterval.toInt, Some(hash))
                case JNull =>
                  Right(GetLatestBlockRequest(chkpInterval.toInt, None))
                case _ =>
                  Left(InvalidParams("Not supported type for parentCheckpoint"))
              }
            else
              Left(InvalidParams("Expected positive integer"))
          case _ =>
            Left(InvalidParams())
        }

      override def encodeJson(resp: GetLatestBlockResponse): JsonAST.JValue =
        Extraction.decompose(resp)(formats - QuantitiesSerializer)
    }

  implicit val checkpointing_pushCheckpoint: JsonMethodCodec[PushCheckpointRequest, PushCheckpointResponse] =
    new JsonMethodCodec[PushCheckpointRequest, PushCheckpointResponse] {
      override def decodeJson(
          params: Option[JsonAST.JArray]
      ): Either[JsonRpcError, PushCheckpointRequest] =
        params match {
          case Some(JArray(JString(hashStr) :: (signatures: JArray) :: Nil)) =>
            for {
              hash <- extractHash(hashStr)
              sigs <- extractSignatures(signatures)
            } yield PushCheckpointRequest(hash, sigs)

          case _ =>
            Left(InvalidParams())
        }

      override def encodeJson(t: PushCheckpointResponse): JsonAST.JValue =
        JBool(true)
    }

  private def extractSignatures(arr: JArray): Either[JsonRpcError, List[ECDSASignature]] = {
    import cats.implicits._
    def parseSig(bs: ByteString) =
      ECDSASignature.fromBytes(bs).toRight(InvalidParams("Bad signature length"))

    arr.arr.traverse {
      case JString(str) => extractBytes(str).flatMap(parseSig)

      case other => Left(InvalidParams(s"Unable to extract a signature from: $other"))
    }
  }
}
