package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.DebugService.{ListPeersInfoRequest, ListPeersInfoResponse}
import io.iohk.ethereum.jsonrpc.JsonRpcController.{Codec, JsonEncoder}
import io.iohk.ethereum.jsonrpc.JsonRpcController.JsonDecoder.NoParamsDecoder
import org.json4s.JsonAST.{JArray, JString, JValue}

object DebugJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val debug_listPeersInfo: Codec[ListPeersInfoRequest, ListPeersInfoResponse] =
    new NoParamsDecoder(ListPeersInfoRequest()) with JsonEncoder[ListPeersInfoResponse] {
      def encodeJson(t: ListPeersInfoResponse): JValue =
        JArray(t.peers.map(a => JString(a.toString)))
    }
}
