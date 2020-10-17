package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.JsonRpcController.Codec
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.jsonrpc.QAService.{MineBlocksRequest, MineBlocksResponse}
import org.json4s.JsonAST._

object QAJsonMethodsImplicits extends JsonMethodsImplicits {
  implicit val qa_mineBlocks: Codec[MineBlocksRequest, MineBlocksResponse] =
    new Codec[MineBlocksRequest, MineBlocksResponse] {
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
}
