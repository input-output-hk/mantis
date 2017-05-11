package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthService.{ProtocolVersionRequest, ProtocolVersionResponse}
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object JsonRpcController {
  trait JsonDecoder[T] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, T]
  }

  trait JsonEncoder[T] {
    def encodeJson(t: T): JValue
  }
}

class JsonRpcController(web3Service: Web3Service, netService: NetService, ethService: EthService) {

  import JsonMethodsImplicits._
  import JsonRpcErrors._

  def handleRequest(request: JsonRpcRequest): Future[JsonRpcResponse] = {
    request.method match {
      case "web3_sha3" => handle[Sha3Request, Sha3Response](web3Service.sha3, request)
      case "web3_clientVersion" => handle[ClientVersionRequest, ClientVersionResponse](web3Service.clientVersion, request)
      case "net_version" => handle[VersionRequest, VersionResponse](netService.version, request)
      case "net_listening" => handle[ListeningRequest, ListeningResponse](netService.listening, request)
      case "net_peerCount" => handle[PeerCountRequest, PeerCountResponse](netService.peerCount, request)
      case "eth_protocolVersion" => handle[ProtocolVersionRequest, ProtocolVersionResponse](ethService.protocolVersion, request)   
      case _ => Future.successful(errorResponse(request, MethodNotFound))
    }
  }

  private def handle[Req, Res](fn: Req => Future[Res], rpcReq: JsonRpcRequest)
                              (implicit dec: JsonDecoder[Req], enc: JsonEncoder[Res]): Future[JsonRpcResponse] = {
    dec.decodeJson(rpcReq.params) match {
      case Right(req) =>
        fn(req)
          .map(successResponse(rpcReq, _))
          .recover { case ex => errorResponse(rpcReq, InternalError) }
      case Left(error) =>
        Future.successful(errorResponse(rpcReq, error))
    }
  }

  private def successResponse[T](req: JsonRpcRequest, result: T)(implicit enc: JsonEncoder[T]): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, Some(enc.encodeJson(result)), None, req.id.getOrElse(0))

  private def errorResponse[T](req: JsonRpcRequest, error: JsonRpcError): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, None, Some(error), req.id.getOrElse(0))

}
