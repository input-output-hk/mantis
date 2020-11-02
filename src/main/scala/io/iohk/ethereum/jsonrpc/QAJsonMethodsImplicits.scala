package io.iohk.ethereum.jsonrpc

import akka.util.ByteString
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.QAService.{MineBlocksRequest, MineBlocksResponse, _}
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec}
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import org.json4s.Extraction
import org.json4s.JsonAST._

object QAJsonMethodsImplicits extends JsonMethodsImplicits {
  implicit val qa_mineBlocks: JsonMethodCodec[MineBlocksRequest, MineBlocksResponse] =
    new JsonMethodCodec[MineBlocksRequest, MineBlocksResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, MineBlocksRequest] = {
        params match {
          case Some(JArray(JInt(numBlocks) :: JBool(withTransactions) :: Nil)) =>
            Right(MineBlocksRequest(numBlocks.toInt, withTransactions))
          case Some(JArray(JInt(numBlocks) :: JBool(withTransactions) :: JNull :: Nil)) =>
            Right(MineBlocksRequest(numBlocks.toInt, withTransactions))

          case Some(JArray(JInt(numBlocks) :: JBool(withTransactions) :: JString(parentBlock) :: Nil)) =>
            for {
              parentBlockHash <- extractBytes(parentBlock)
            } yield MineBlocksRequest(numBlocks.toInt, withTransactions, Some(parentBlockHash))
          case _ =>
            Left(InvalidParams())
        }
      }

      def encodeJson(t: MineBlocksResponse): JValue = JObject(
        "responseType" -> JString(t.responseType.entryName),
        "message" -> t.message.fold[JValue](JNull)(JString)
      )
    }

  implicit val qa_generateCheckpoint: JsonMethodCodec[GenerateCheckpointRequest, GenerateCheckpointResponse] =
    new JsonMethodCodec[GenerateCheckpointRequest, GenerateCheckpointResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, GenerateCheckpointRequest] = {
        params match {
          case Some(JArray((keys: JArray) :: JString(hash) :: Nil)) =>
            for {
              blockHash <- extractHash(hash)
              keys <- parseKeysList(keys)
            } yield GenerateCheckpointRequest(keys, Some(blockHash))
          case Some(JArray((keys: JArray) :: Nil)) =>
            parseKeysList(keys).map(GenerateCheckpointRequest(_, None))
          case _ =>
            Left(InvalidParams())
        }
      }

      def encodeJson(t: GenerateCheckpointResponse): JValue = Extraction.decompose(t.checkpoint)
    }

  private def parseKeysList(arr: JArray): Either[JsonRpcError, List[ByteString]] = {
    import cats.implicits._
    arr.arr.traverse {
      case JString(key) => extractBytes(key)
      case other => Left(InvalidParams(msg = s"Unable to parse private key, expected byte data but got: $other"))
    }
  }

  implicit val qa_getFederationMembersInfo
      : JsonMethodCodec[GetFederationMembersInfoRequest, GetFederationMembersInfoResponse] =
    new NoParamsMethodDecoder(GetFederationMembersInfoRequest()) with JsonEncoder[GetFederationMembersInfoResponse] {
      def encodeJson(t: GetFederationMembersInfoResponse): JValue = Extraction.decompose(t)
    }
}
