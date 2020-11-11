package io.iohk.ethereum.faucet

import akka.actor.{ActorRef, ActorSystem, OneForOneStrategy, SupervisorStrategy}
import akka.pattern.{BackoffOpts, BackoffSupervisor}
import io.iohk.ethereum.faucet.jsonrpc.WalletService
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration._

object FaucetSupervisor {
  val name = "FaucetSupervisor"
}

class FaucetSupervisor(walletRpcClient: WalletService, config: FaucetConfig)(implicit system: ActorSystem)
    extends Logger {

  val childProps = FaucetHandler.props(walletRpcClient, config)

  val minBackoff: FiniteDuration = config.supervisor.minBackoff
  val maxBackoff: FiniteDuration = config.supervisor.maxBackoff
  val randomFactor: Double = config.supervisor.randomFactor
  val autoReset: FiniteDuration = config.supervisor.autoReset

  val supervisorProps = BackoffSupervisor.props(
    BackoffOpts
      .onFailure(
        childProps,
        childName = FaucetHandler.name,
        minBackoff = minBackoff,
        maxBackoff = maxBackoff,
        randomFactor = randomFactor
      )
      .withAutoReset(autoReset)
      .withSupervisorStrategy(OneForOneStrategy() { case error ⇒
        log.error("Restart Consumer", error)
        SupervisorStrategy.Restart
      })
  )
  val supervisor: ActorRef = system.actorOf(supervisorProps, FaucetSupervisor.name)
}
