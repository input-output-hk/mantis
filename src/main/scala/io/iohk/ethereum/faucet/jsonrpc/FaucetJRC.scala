package io.iohk.ethereum.faucet.jsonrpc

import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain._
import io.iohk.ethereum.jsonrpc.JsonRpcController.Apis
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcControllerCommon.JsonRpcConfig
import io.iohk.ethereum.jsonrpc.server.controllers.JsonRpcControllerCommon
import io.iohk.ethereum.jsonrpc.{JsonRpcRequest, JsonRpcResponse}

import scala.concurrent.Future

class FaucetJRC(
    faucetRpcService: FaucetRpcService,
  override val config: JsonRpcConfig
  ) extends JsonRpcControllerCommon {

  override def enabledApis: Seq[String] = config.apis :+ Apis.Rpc

  override def handleFn: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]] = Map(
    Apis.Eth -> handleFaucetRequest,
  )

  def handleFaucetRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, FaucetJRC.SendFunds, _, _) =>
      handle[SendFundsRequest, SendFundsResponse](faucetRpcService.sendFunds, req)
    case req @ JsonRpcRequest(_, FaucetJRC.Status, _, _) =>
      handle[StatusRequest, StatusResponse](faucetRpcService.status, req)
  }
}

object FaucetJRC {
  private val Prefix = "faucet_"

  val SendFunds: String = Prefix + "sendFunds"
  val Status: String = Prefix + "status"
}

