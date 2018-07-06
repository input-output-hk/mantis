package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.DebugService.{ ListPeersInfoRequest, ListPeersInfoResponse }
import org.json4s.JsonAST.{ JArray, JString, JValue }

object DebugJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val debug_listPeersInfo: Codec[ListPeersInfoRequest, ListPeersInfoResponse] =
    new Codec[ListPeersInfoRequest, ListPeersInfoResponse] {
      def decodeJson(params: Option[JArray]): Either[JsonRpcError, ListPeersInfoRequest] =
        Right(ListPeersInfoRequest())

      def encodeJson(t: ListPeersInfoResponse): JValue =
        JArray(t.peers.map(a => JString(a.toString)))
    }
}
