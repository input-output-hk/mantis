package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.faucet.jsonrpc.FaucetJRC.Apis
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcControllerCommon
import io.iohk.ethereum.jsonrpc.{JsonRpcError, JsonRpcRequest, JsonRpcResponse}

import scala.concurrent.Future

class FaucetJsonRpcController(
    faucetHandleFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]], //TODO.... map?
    override val config: JsonRpcControllerCommon.JsonRpcConfig
) extends JsonRpcControllerCommon {

  override def enabledApis: Seq[String] = config.apis :+ Apis.Rpc //TODO: add param

  override def apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]] = Map(
    Apis.Faucet -> handleFaucetRequest
  )

  def handleFaucetRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = { case req =>
    val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = { case _ =>
      Future.successful(errorResponse(req, JsonRpcError.MethodNotFound))
    }
    (faucetHandleFn orElse notFoundFn)(req)
  }
}
