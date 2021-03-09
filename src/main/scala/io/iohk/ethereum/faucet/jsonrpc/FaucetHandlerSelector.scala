package io.iohk.ethereum.faucet.jsonrpc

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.RetrySupport
import akka.util.Timeout
import io.iohk.ethereum.faucet.{FaucetConfigBuilder, FaucetHandler, FaucetSupervisor}
import monix.eval.Task

trait FaucetHandlerSelector {
  self: FaucetConfigBuilder with RetrySupport =>

  val handlerPath: String = s"user/${FaucetSupervisor.name}/${FaucetHandler.name}"
  lazy val attempts = faucetConfig.supervisor.attempts
  lazy val delay = faucetConfig.supervisor.delay

  lazy val handlerTimeout: Timeout = Timeout(faucetConfig.handlerTimeout)

  def selectFaucetHandler()(implicit system: ActorSystem): Task[ActorRef] =
    Task.deferFuture(
      retry(() => system.actorSelection(handlerPath).resolveOne(handlerTimeout.duration), attempts, delay)(
        system.dispatcher,
        system.scheduler
      )
    )

}
