package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST.{JArray, JValue}

case class JsonRpcRequest(jsonrpc: String, method: String, params: Option[JArray], id: Option[JValue])
