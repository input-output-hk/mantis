package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST.JValue

case class JsonRpcError(code: Int, message: String, data: Option[JValue])

// scalastyle:off magic.number
object JsonRpcErrors {
  val ParseError = JsonRpcError(-32700, "An error occurred on the server while parsing the JSON text", None)
  val InvalidRequest = JsonRpcError(-32600, "The JSON sent is not a valid Request object", None)
  val MethodNotFound = JsonRpcError(-32601, "The method does not exist / is not available", None)
  val InvalidParams = JsonRpcError(-32602, "Invalid method parameters", None)
  val InternalError = JsonRpcError(-32603, "Internal JSON-RPC error", None)
}
