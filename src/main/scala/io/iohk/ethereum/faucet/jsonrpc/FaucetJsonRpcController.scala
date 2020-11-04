package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain._
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcBaseController.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.{JsonRpcError, JsonRpcRequest, JsonRpcResponse}
import io.iohk.ethereum.utils.Logger

import scala.concurrent.Future

class FaucetJsonRpcController(
    faucetRpcService: FaucetRpcService,
    override val config: JsonRpcConfig
) extends ApisBuilder
    with Logger
    with JsonRpcBaseController {

  override def enabledApis: Seq[String] = config.apis

  override def apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]] = Map(
    Apis.Faucet -> handleRequest
  )

  def handleRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = { case req =>
    val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = { case _ =>
      Future.successful(errorResponse(req, JsonRpcError.MethodNotFound))
    }
    (handleFaucetRequest orElse notFoundFn)(req)
  }

  private def handleFaucetRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, FaucetJsonRpcController.SendFunds, _, _) =>
      handle[SendFundsRequest, SendFundsResponse](faucetRpcService.sendFunds, req)
    case req @ JsonRpcRequest(_, FaucetJsonRpcController.Status, _, _) =>
      handle[StatusRequest, StatusResponse](faucetRpcService.status, req)
  }
}

object FaucetJsonRpcController {
  private val Prefix = "faucet_"

  val SendFunds: String = Prefix + "sendFunds"
  val Status: String = Prefix + "status"

}
