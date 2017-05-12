package io.iohk.ethereum.jsonrpc

import io.iohk.ethereum.jsonrpc.EthService.{ProtocolVersionRequest, ProtocolVersionResponse, SyncingRequest, SyncingResponse}
import io.iohk.ethereum.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import io.iohk.ethereum.jsonrpc.NetService._
import io.iohk.ethereum.jsonrpc.Web3Service.{ClientVersionRequest, ClientVersionResponse, Sha3Request, Sha3Response}
import io.iohk.ethereum.utils.Config
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
  import Config.Network.{Rpc => RpcConfig}

  val apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]] = Map(
    RpcConfig.Apis.Eth -> handleEthRequest,
    RpcConfig.Apis.Web3 -> handleWeb3Request,
    RpcConfig.Apis.Net -> handleNetRequest,
    RpcConfig.Apis.Db -> PartialFunction.empty,
    RpcConfig.Apis.Personal -> PartialFunction.empty,
    RpcConfig.Apis.Admin -> PartialFunction.empty,
    RpcConfig.Apis.Debug -> PartialFunction.empty
  )

  private def handleWeb3Request: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "web3_sha3", _, _) => handle[Sha3Request, Sha3Response](web3Service.sha3, req)
    case req @ JsonRpcRequest(_, "web3_clientVersion", _, _) => handle[ClientVersionRequest, ClientVersionResponse](web3Service.clientVersion, req)
  }

  private def handleNetRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "net_version", _, _) => handle[VersionRequest, VersionResponse](netService.version, req)
    case req @ JsonRpcRequest(_, "net_listening", _, _) => handle[ListeningRequest, ListeningResponse](netService.listening, req)
    case req @ JsonRpcRequest(_, "net_peerCount", _, _) => handle[PeerCountRequest, PeerCountResponse](netService.peerCount, req)
  }

  private def handleEthRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "eth_protocolVersion", _, _) => handle[ProtocolVersionRequest, ProtocolVersionResponse](ethService.protocolVersion, req)
    case req @ JsonRpcRequest(_, "eth_syncing", _, _) => handle[SyncingRequest, SyncingResponse](ethService.syncing, req)

  }

  def handleRequest(request: JsonRpcRequest): Future[JsonRpcResponse] = {
    val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
      case _ => Future.successful(errorResponse(request, MethodNotFound))
    }

    val handleFn = RpcConfig.apis.foldLeft(notFoundFn)((fn, api) => apisHandleFns.getOrElse(api, PartialFunction.empty) orElse fn)

    handleFn(request)
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
