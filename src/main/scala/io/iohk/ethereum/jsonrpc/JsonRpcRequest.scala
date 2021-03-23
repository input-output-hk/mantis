package io.iohk.ethereum.jsonrpc

import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.native.Serialization.write
import org.json4s.DefaultFormats
import org.json4s.Formats

//TODO: work on a more elegant solution
trait SensitiveInformationToString {
  val method: String

  def toStringWithSensitiveInformation: String = {
    if (!method.contains("personal"))
      toString
    else "sensitive information"
  }
}

case class JsonRpcRequest(jsonrpc: String, method: String, params: Option[JArray], id: Option[JValue])
    extends SensitiveInformationToString {
  override def toString(): String = {
    implicit val formats: Formats = DefaultFormats
    "JsonRpcRequest" + (jsonrpc, method, params.map(write(_)), id.map(write(_))).toString
  }
}
