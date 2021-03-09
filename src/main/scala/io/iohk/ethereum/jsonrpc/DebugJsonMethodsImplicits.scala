package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.DebugService.{ListPeersInfoRequest, ListPeersInfoResponse}
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder
import io.iohk.ethereum.jsonrpc.serialization.{JsonEncoder, JsonMethodCodec}
import org.json4s.JsonAST.{JArray, JString, JValue}

object DebugJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val debug_listPeersInfo: JsonMethodCodec[ListPeersInfoRequest, ListPeersInfoResponse] =
    new NoParamsMethodDecoder(ListPeersInfoRequest()) with JsonEncoder[ListPeersInfoResponse] {
      def encodeJson(t: ListPeersInfoResponse): JValue =
        JArray(t.peers.map(a => JString(a.toString)))
    }
}
