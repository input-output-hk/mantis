package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST.{JArray, JValue}

//TODO: work on a more elegant solution
trait SensitiveInformationToString {
  val method: String

  def toStringWithSensitiveInformation: String =
    if (!method.contains("personal"))
      toString
    else "sensitive information"
}

case class JsonRpcRequest(jsonrpc: String, method: String, params: Option[JArray], id: Option[JValue])
    extends SensitiveInformationToString
