package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.jsonrpc.JsonRpcController.Apis
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.MethodNotFound
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcControllerCommon
import io.iohk.ethereum.jsonrpc.{JsonRpcRequest, JsonRpcResponse}

import scala.concurrent.Future

class FaucetJsonRpcController(
                               faucetHandleFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]],
                               override val config: JsonRpcControllerCommon.JsonRpcConfig
                             ) extends JsonRpcControllerCommon {

  override def enabledApis: Seq[String] = config.apis :+ Apis.Rpc //TODO: add param

  override def handleFn: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]] = Map(
    Apis.Eth -> handleFaucetRequest,
  )

  def handleFaucetRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req =>
      val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
        case _ => Future.successful(errorResponse(req, MethodNotFound))
      }
      (faucetHandleFn orElse notFoundFn)(req)
  }
}
