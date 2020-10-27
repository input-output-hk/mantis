package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.jsonrpc.{JsonRpcController, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.MethodNotFound

import scala.concurrent.Future

class FaucetJsonRpcController(
                               faucetHandleFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]],
                               override val config: JsonRpcController.JsonRpcConfig
                             ) extends JsonRpcControllerCommon {

  override def handleFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req =>
      val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
        case _ => Future.successful(errorResponse(req, MethodNotFound))
      }
      (faucetHandleFn orElse notFoundFn)(req)
  }
}
