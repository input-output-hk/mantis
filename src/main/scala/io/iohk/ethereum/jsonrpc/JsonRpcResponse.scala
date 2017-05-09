package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST.JValue

case class JsonRpcResponse(jsonrpc: String, result: Option[JValue], error: Option[JsonRpcError], id: JValue)
