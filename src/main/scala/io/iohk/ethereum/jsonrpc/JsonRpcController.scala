package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.Web3Service._
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL._

import scala.concurrent.Future
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

object JsonRpcController {
  trait JsonDecoder[T] {
    def decodeJson(params: JArray): Try[T]
  }

  trait JsonEncoder[T] {
    def encodeJson(t: T): JValue
  }
}

class JsonRpcController(web3Service: Web3Service) {

  import JsonMethodsImplicits._

  def handleRequest(request: JsonRpcRequest): Future[JsonRpcResponse] = {
    request.method match {
      case "web3_sha3" => handle[Sha3Request, Sha3Response](web3Service.sha3, request)
      case "web3_clientVersion" => handle[ClientVersionRequest, ClientVersionResponse](web3Service.clientVersion, request)

      case "eth_submitHashrate" => handle[HashRateSubmitRequest, HashRateSubmitResponse](web3Service.submitHashRate, request)
      case "eth_getWork" => handle[GetWorkRequest, GetWorkResponse](web3Service.getWork, request)
      case "eth_submitWork" => handle[GetWorkRequest, GetWorkResponse](web3Service.getWork, request)
      case "eth_awaitNewWork" => handle[GetWorkRequest, GetWorkResponse](web3Service.getWork, request)
      case "eth_progress" => handle[GetWorkRequest, GetWorkResponse](web3Service.getWork, request)

      case _ => Future.successful {
        JsonRpcResponse(request.jsonrpc, None, Some(JsonRpcError(0, s"Unknown RPC method ${request.method}", None)), request.id.getOrElse(1))
      }
    }
  }

  private def handle[Req, Res](fn: Req => Future[Res], rpcReq: JsonRpcRequest)
                              (implicit dec: JsonDecoder[Req], enc: JsonEncoder[Res]): Future[JsonRpcResponse] = {
    val req = dec.decodeJson(rpcReq.params.getOrElse(JArray(Nil))).get
    fn(req) map { res =>
      JsonRpcResponse(rpcReq.jsonrpc, Some(enc.encodeJson(res)), None, rpcReq.id.getOrElse(0))
    } recover { case ex =>
      JsonRpcResponse(rpcReq.jsonrpc, None, Some(JsonRpcError(0, ex.getMessage, None)), rpcReq.id.getOrElse(0))
    }
  }

}
