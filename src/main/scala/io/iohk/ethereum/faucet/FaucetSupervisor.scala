package io.iohk.ethereum.faucet

import akka.actor.{ActorRef, ActorSelection, ActorSystem, OneForOneStrategy, SupervisorStrategy}
import akka.pattern.{BackoffOpts, BackoffSupervisor}
import io.iohk.ethereum.faucet.jsonrpc.WalletRpcClient
import io.iohk.ethereum.utils.Logger

import scala.concurrent.duration._

class FaucetSupervisor(walletRpcClient: WalletRpcClient, config: FaucetConfig)(implicit system: ActorSystem) extends Logger{

  val childProps = FaucetHandler.props(walletRpcClient, config)

  //TODO: add in config
  val minBackoff: FiniteDuration = 3.seconds
  val maxBackoff: FiniteDuration = 30.seconds
  val randomFactor: Double = 0.2
  val autoReset: FiniteDuration = 10.seconds

  val supervisorProps = BackoffSupervisor.props(
    BackoffOpts.onFailure(
      childProps,
      childName = "FaucetHandler",
      minBackoff = minBackoff,
      maxBackoff = maxBackoff,
      randomFactor = randomFactor
    ).withAutoReset(autoReset)
      .withSupervisorStrategy(
      OneForOneStrategy() {
        case error ⇒
          log.error("Restart Consumer", error)
          SupervisorStrategy.Restart
      })
  )
  val supervisor: ActorRef = system.actorOf(supervisorProps, "FaucetSupervisor")
  val flowConsumer: ActorSelection = system.actorSelection(supervisor.path + "/FaucetHandler")
}
