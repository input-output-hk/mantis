package io.iohk.ethereum.faucet.jsonrpc

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.RetrySupport
import akka.util.Timeout
import io.iohk.ethereum.faucet.{FaucetConfig, FaucetHandler, FaucetSupervisor}
import io.iohk.ethereum.faucet.FaucetHandler.{FaucetHandlerMsg, FaucetHandlerResponse}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.{SendFundsRequest, SendFundsResponse, StatusRequest, StatusResponse}
import io.iohk.ethereum.jsonrpc.{JsonRpcError, ServiceResponse}
import io.iohk.ethereum.utils.Logger
import monix.eval.Task

import scala.concurrent.ExecutionContext


class FaucetRpcService(config: FaucetConfig)(implicit system: ActorSystem) extends RetrySupport with Logger {

  implicit val scheduler = system.scheduler
  val actorSelected = system.actorSelection(s"user/${FaucetSupervisor.name}/${FaucetHandler.name}")
  val attempts = config.supervisor.attempts
  val delay = config.supervisor.delay

  lazy val handlerTimeout: Timeout = Timeout(config.handlerTimeout)
  implicit lazy val actorTimeout: Timeout = Timeout(config.responseTimeout)
  implicit val ec: ExecutionContext = system.dispatcher

  private def faucetHandler(): Task[ActorRef] =
    Task.deferFuture(retry(() => actorSelected.resolveOne(handlerTimeout.duration), attempts, delay))

  def sendFunds(sendFundsRequest: SendFundsRequest): ServiceResponse[SendFundsResponse] =
    faucetHandler().flatMap(handler =>
      handler
        .askFor[Any](FaucetHandlerMsg.SendFunds(sendFundsRequest.address))
        .map(handleSendFundsResponse orElse handleErrors)
    )

  def status(statusRequest: StatusRequest): ServiceResponse[StatusResponse] =
    faucetHandler()
      .flatMap(handler => handler.askFor[Any](FaucetHandlerMsg.Status))
      .map(handleStatusResponse orElse handleErrors)

  private def handleSendFundsResponse: PartialFunction[Any, Either[JsonRpcError, SendFundsResponse]] = {
    case FaucetHandlerResponse.TransactionSent(txHash) =>
      Right(SendFundsResponse(txHash))
  }

  private def handleStatusResponse: PartialFunction[Any, Either[JsonRpcError, StatusResponse]] = {
    case FaucetHandlerResponse.StatusResponse(status) =>
      Right(StatusResponse(status))
  }

  private def handleErrors[T]: PartialFunction[Any, Either[JsonRpcError, T]] = {
    case FaucetHandlerResponse.FaucetIsUnavailable =>
      Left(JsonRpcError.LogicError("Faucet is unavailable: Please try again in a few more seconds"))

    case FaucetHandlerResponse.WalletRpcClientError(error) =>
      Left(JsonRpcError.LogicError(s"Faucet error: $error"))
  }
}
