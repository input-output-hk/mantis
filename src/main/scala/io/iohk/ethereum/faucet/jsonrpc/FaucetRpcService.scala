package io.iohk.ethereum.faucet.jsonrpc

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.RetrySupport
import akka.util.Timeout
import io.iohk.ethereum.faucet.FaucetConfig
import io.iohk.ethereum.faucet.FaucetHandler.{FaucetHandlerMsg, FaucetHandlerResponse}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.{SendFundsRequest, SendFundsResponse, StatusRequest, StatusResponse}
import io.iohk.ethereum.jsonrpc.{JsonRpcError, ServiceResponse}
import io.iohk.ethereum.utils.Logger
import monix.eval.Task

import scala.concurrent.ExecutionContext.Implicits.global

class FaucetRpcService(config: FaucetConfig)(implicit system: ActorSystem)  extends RetrySupport with Logger {

  implicit val scheduler = system.scheduler
  val actorSelected = system.actorSelection("/user/FaucetSupervisor/FaucetHandler")
  val attempts = 4 //configuration.get[Int]("token.supervisor.future.attempts") //TODO add
  val delay = config.responseTimeout //configuration.get[Int]("token.supervisor.future.delay") //TODO change val

  implicit lazy val timeout: Timeout = Timeout(config.responseTimeout)

  private def faucetHandler(): Task[ActorRef] = Task.deferFuture(
    retry (
      () => actorSelected.resolveOne(timeout.duration),
      attempts,
      delay))

  def sendFunds(sendFundsRequest: SendFundsRequest): ServiceResponse[SendFundsResponse] =
    faucetHandler().flatMap( handler =>
      handler.askFor[SendFundsResponse](FaucetHandlerMsg.SendFunds(sendFundsRequest.address)
      ).map(handleSendFundsResponse orElse handleErrors)
    )

  def status(statusRequest: StatusRequest): ServiceResponse[StatusResponse] =
    faucetHandler().flatMap( handler =>
      handler.askFor[StatusResponse](FaucetHandlerMsg.Status)
    ).map(handleStatusResponse orElse handleErrors)

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
  }
}