package io.iohk.ethereum.jsonrpc

import akka.util.ByteString

import org.json4s.JsonAST.JBool
import org.json4s.JsonAST.JDecimal
import org.json4s.JsonAST.JInt
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.utils.ByteStringUtils

trait JRCMatchers extends Matchers {

  class VersionMatcher private[JRCMatchers] (expectedVersion: String) extends Matcher[JsonRpcResponse] {
    def apply(response: JsonRpcResponse): MatchResult =
      MatchResult(
        response.jsonrpc.equals(expectedVersion),
        s"Version ${response.jsonrpc} did not equal to $expectedVersion",
        s"Version ${response.jsonrpc} was equal to $expectedVersion"
      )
  }

  class IdMatcher private[JRCMatchers] (expectedId: JInt) extends Matcher[JsonRpcResponse] {
    def apply(response: JsonRpcResponse): MatchResult =
      MatchResult(
        response.id.equals(expectedId),
        s"Id ${response.id} did not equal to $expectedId",
        s"Id ${response.id} was equal to $expectedId"
      )
  }

  class ErrorMatcher private[JRCMatchers] (expectedError: Option[JsonRpcError]) extends Matcher[JsonRpcResponse] {
    def apply(response: JsonRpcResponse): MatchResult =
      MatchResult(
        response.error.equals(expectedError),
        s"Error ${response.error} did not equal to $expectedError",
        s"Error ${response.error} was equal to $expectedError"
      )
  }

  class ResultMatcher private[JRCMatchers] (expectedResult: Option[JValue]) extends Matcher[JsonRpcResponse] {
    def apply(response: JsonRpcResponse): MatchResult =
      MatchResult(
        response.result.equals(expectedResult),
        s"Result ${response.result} did not equal to $expectedResult",
        s"Result ${response.result} was equal to $expectedResult"
      )
  }

  class ResponseMatcher private[JRCMatchers] (
      jsonrpc: String,
      id: JInt,
      error: Option[JsonRpcError],
      result: Option[JValue]
  ) extends Matcher[JsonRpcResponse] {
    def apply(left: JsonRpcResponse): MatchResult =
      new VersionMatcher(jsonrpc)
        .and(new IdMatcher(id))
        .and(new ErrorMatcher(error))
        .and(new ResultMatcher(result))(left)
  }

  def haveBooleanResult(result: Boolean): ResponseMatcher = haveResult(result = JBool(result))
  def haveStringResult(result: String): ResponseMatcher = haveResult(result = JString(result))
  def haveDecimalResult(result: BigDecimal): ResponseMatcher = haveResult(result = JDecimal(result))
  def haveByteStringResult(result: ByteString): ResponseMatcher =
    haveResult(result = JString("0x" + ByteStringUtils.hash2string(result)))
  def haveObjectResult(items: (String, JValue)*): ResponseMatcher = haveResult(result = JObject(items: _*))
  def haveResult(result: JValue): ResponseMatcher = beResponse(result = Some(result))
  def haveError(expectedError: JsonRpcError): ResponseMatcher = beResponse(error = Some(expectedError))

  def beResponse(
      jsonrpc: String = "2.0",
      id: JInt = JInt(1),
      error: Option[JsonRpcError] = None,
      result: Option[JValue] = None
  ): ResponseMatcher = new ResponseMatcher(jsonrpc, id, error, result)

}

object JRCMatchers extends JRCMatchers
