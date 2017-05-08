package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST.JValue

case class JsonRpcError(code: Int, message: String, data: Option[JValue])
