package io.iohk.ethereum.jsonrpc

import io.circe.Json.JNumber
import io.iohk.ethereum.jsonrpc.DebugService.{
  GetSpecificBlockRequest,
  GetSpecificBlockResponse,
  ListPeersInfoRequest,
  ListPeersInfoResponse
}
import io.iohk.ethereum.jsonrpc.JsonRpcError.{InvalidParams, ParseError}
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec}
import org.json4s.JsonAST._

object DebugJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val debug_listPeersInfo: JsonMethodCodec[ListPeersInfoRequest, ListPeersInfoResponse] =
    new NoParamsMethodDecoder(ListPeersInfoRequest()) with JsonEncoder[ListPeersInfoResponse] {
      def encodeJson(t: ListPeersInfoResponse): JValue =
        JArray(t.peers.map(a => JString(a.toString)))
    }

  implicit val debug_getSpecificBlockRequest: JsonMethodCodec[GetSpecificBlockRequest, GetSpecificBlockResponse] =
    new JsonMethodCodec[GetSpecificBlockRequest, GetSpecificBlockResponse] {
      override def decodeJson(
          params: Option[JArray]
      ): Either[JsonRpcError, GetSpecificBlockRequest] =
        params match {
          case Some(JArray(JInt(blockNumber) :: Nil)) =>
            Right(GetSpecificBlockRequest(blockNumber.bigInteger))
          case _ =>
            Left(InvalidParams())
        }

      override def encodeJson(t: GetSpecificBlockResponse): JValue = JBool(true)
    }
}
