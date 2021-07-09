package io.iohk.ethereum.jsonrpc

import org.json4s.JInt
import org.json4s.JLong
import org.json4s.JObject
import org.json4s.JString
import org.json4s.JValue

import io.iohk.ethereum.consensus.mining.Protocol
import io.iohk.ethereum.jsonrpc.serialization.JsonEncoder

case class JsonRpcError(code: Int, message: String, data: Option[JValue])

// scalastyle:off magic.number
// scalastyle:off public.methods.have.type
object JsonRpcError extends JsonMethodsImplicits {

  def apply[T: JsonEncoder](code: Int, message: String, data: T): JsonRpcError =
    JsonRpcError(code, message, Some(JsonEncoder[T].encodeJson(data)))

  implicit val rateLimitInformation: JsonEncoder[RateLimitInformation] = (rateLimit: RateLimitInformation) =>
    JObject(
      "backoff_seconds" -> JLong(rateLimit.backoffSeconds)
    )

  implicit val jsonRpcErrorEncoder: JsonEncoder[JsonRpcError] = err =>
    JObject(
      List("code" -> JsonEncoder.encode(err.code), "message" -> JsonEncoder.encode(err.message)) ++
        err.data.map("data" -> _)
    )

  case class RateLimitInformation(backoffSeconds: Long)
  def RateLimitError(backoffSeconds: Long): JsonRpcError =
    JsonRpcError(-32005, "request rate exceeded", RateLimitInformation(backoffSeconds))
  val ParseError: JsonRpcError =
    JsonRpcError(-32700, "An error occurred on the server while parsing the JSON text", None)
  val InvalidRequest: JsonRpcError = JsonRpcError(-32600, "The JSON sent is not a valid Request object", None)
  val MethodNotFound: JsonRpcError = JsonRpcError(-32601, "The method does not exist / is not available", None)
  def InvalidParams(msg: String = "Invalid method parameters"): JsonRpcError = JsonRpcError(-32602, msg, None)
  val InternalError: JsonRpcError = JsonRpcError(-32603, "Internal JSON-RPC error", None)
  def LogicError(msg: String): JsonRpcError = JsonRpcError(-32000, msg, None)
  val AccountLocked: JsonRpcError = LogicError("account is locked or unknown")

  // We use the recommendation from https://eth.wiki/json-rpc/json-rpc-error-codes-improvement-proposal
  //
  // Note Error Code "2", "Action not allowed" could be a candidate here, but the description they provide
  //      probably does not match this use-case.
  final val MiningIsNotEthash: JsonRpcError =
    JsonRpcError(200, s"The mining algorithm is not ${Protocol.Names.PoW}", None)

  def executionError(reasons: List[EthCustomError]): JsonRpcError = JsonRpcError(3, "Execution error", reasons)
  val NodeNotFound: JsonRpcError = executionError(List(EthCustomError.DoesntExist("State node")))
  val BlockNotFound: JsonRpcError = executionError(List(EthCustomError.DoesntExist("Block")))

  // Custom errors based on proposal https://eth.wiki/json-rpc/json-rpc-error-codes-improvement-proposal
  sealed abstract class EthCustomError private (val code: Int, val message: String)
  object EthCustomError {
    implicit val ethCustomErrorEncoder: JsonEncoder[EthCustomError] = err =>
      JObject("code" -> JInt(err.code), "message" -> JString(err.message))

    case class DoesntExist(what: String) extends EthCustomError(100, s"${what} doesn't exist")
    case object RequiresEther extends EthCustomError(101, "Requires ether")
    case object GasTooLow extends EthCustomError(102, "Gas too low")
    case object GasLimitExceeded extends EthCustomError(103, "Gas limit exceeded")
    case object Rejected extends EthCustomError(104, "Rejected")
    case object EtherTooLow extends EthCustomError(105, "Ether too low")
  }
}
