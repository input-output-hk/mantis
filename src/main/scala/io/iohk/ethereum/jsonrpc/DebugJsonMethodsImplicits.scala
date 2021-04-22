package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.DebugService.{
  GetSpecificBlockRequest,
  GetSpecificBlockResponse,
  ListPeersInfoRequest,
  ListPeersInfoResponse
}
import io.iohk.ethereum.jsonrpc.JsonRpcError.InvalidParams
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec}
import io.iohk.ethereum.network.PeerId
import org.json4s
import org.json4s.JsonAST
import org.json4s.JsonAST.{JArray, JString, JValue, _}

object DebugJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val debug_listPeersInfo: JsonMethodCodec[ListPeersInfoRequest, ListPeersInfoResponse] =
    new NoParamsMethodDecoder(ListPeersInfoRequest()) with JsonEncoder[ListPeersInfoResponse] {
      def encodeJson(t: ListPeersInfoResponse): JValue =
        JArray(t.peers.map(a => JString(a.toString)))
    }

  implicit val debug_getSpecificBlockRequest: JsonMethodCodec[GetSpecificBlockRequest, GetSpecificBlockResponse] =
    new JsonMethodCodec[GetSpecificBlockRequest, GetSpecificBlockResponse] {
      override def decodeJson(
          params: Option[JsonAST.JArray]
      ): Either[JsonRpcError, GetSpecificBlockRequest] =
        params match {
          case Some(JArray(JString(blockHash) :: JString(peerId) :: Nil)) =>
            for {
              hash <- extractHash(blockHash)
            } yield GetSpecificBlockRequest(hash, PeerId(peerId))
          case _ =>
            Left(InvalidParams())
        }

      override def encodeJson(t: GetSpecificBlockResponse): json4s.JValue = JBool(true)
    }
}
