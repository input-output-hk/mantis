package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.jsonrpc.CheckpointingService._
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodCodec
import io.iohk.ethereum.jsonrpc.serialization.JsonSerializers.QuantitiesSerializer
import org.json4s.JsonAST._
import org.json4s.{Extraction, JsonAST}

object CheckpointingJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val checkpointing_getLatestBlock: JsonMethodCodec[GetLatestBlockRequest, GetLatestBlockResponse] =
    new JsonMethodCodec[GetLatestBlockRequest, GetLatestBlockResponse] {
      override def decodeJson(
          params: Option[JsonAST.JArray]
      ): Either[JsonRpcError, GetLatestBlockRequest] =
        params match {
          case Some(JArray(JInt(chkpInterval) :: JString(blockHash) :: Nil)) =>
            if (chkpInterval > 0 && chkpInterval <= Int.MaxValue)
              for {
                hash <- extractHash(blockHash)
              } yield GetLatestBlockRequest(chkpInterval.toInt, Some(hash))
            else
              Left(InvalidParams("Expected positive integer"))
          case Some(JArray(JInt(chkpInterval) :: Nil)) =>
            if (chkpInterval > 0 && chkpInterval <= Int.MaxValue)
              Right(GetLatestBlockRequest(chkpInterval.toInt, None))
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
