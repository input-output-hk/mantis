package io.iohk.ethereum.faucet.jsonrpc

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.pattern.RetrySupport
import akka.util.Timeout

import monix.eval.Task

import io.iohk.ethereum.faucet.FaucetConfigBuilder
import io.iohk.ethereum.faucet.FaucetHandler
import io.iohk.ethereum.faucet.FaucetSupervisor

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
