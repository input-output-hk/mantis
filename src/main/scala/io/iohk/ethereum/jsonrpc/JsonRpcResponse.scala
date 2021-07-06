package io.iohk.ethereum.jsonrpc

import org.json4s.DefaultFormats
import org.json4s.Formats
import org.json4s.JsonAST.JValue
import org.json4s.native.Serialization.write

case class JsonRpcResponse(jsonrpc: String, result: Option[JValue], error: Option[JsonRpcError], id: JValue) {
  def inspect: String = {
    implicit val formats: Formats = DefaultFormats
    "JsonRpcResponse" + (jsonrpc, result.map(write(_)), error.map(write(_))).toString
  }
}
