package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue

import io.iohk.ethereum.jsonrpc.DebugService.ListPeersInfoRequest
import io.iohk.ethereum.jsonrpc.DebugService.ListPeersInfoResponse
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodCodec
import io.iohk.ethereum.jsonrpc.serialization.JsonMethodDecoder.NoParamsMethodDecoder

object DebugJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val debug_listPeersInfo: JsonMethodCodec[ListPeersInfoRequest, ListPeersInfoResponse] =
    new NoParamsMethodDecoder(ListPeersInfoRequest()) with JsonEncoder[ListPeersInfoResponse] {
      def encodeJson(t: ListPeersInfoResponse): JValue =
        JArray(t.peers.map(a => JString(a.toString)))
    }
}
