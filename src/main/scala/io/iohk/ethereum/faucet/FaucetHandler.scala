package io.iohk.ethereum.faucet

import akka.actor.{Actor, ActorLogging, Props}
import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.faucet.FaucetStatus.WalletAvailable
import io.iohk.ethereum.keystore.Wallet
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

class FaucetHandler(walletRpcClient: WalletService, config: FaucetConfig) extends Actor with ActorLogging {

  private val initializationRetryDelay = config.initializationRetryDelay
  private val initializationMaxRetries = config.initializationMaxRetries

  import FaucetHandler.FaucetHandlerMsg._
  import FaucetHandler.FaucetHandlerResponse._

  var wallet: Wallet = _

  override def preStart(): Unit = {
    log info "||=== Faucet Handler Hook ===||"
    self ! Initialization
  }

  override def receive: Receive = unavailable()

  private def unavailable(currentRetries: Int = 0): Receive = {
    case Status =>
      sender() ! StatusResponse(FaucetStatus.FaucetUnavailable)

    case Initialization => {
      log.info("Initialization called (faucet unavailable)")
      val walletIsAvailable = walletInitialization.runSyncUnsafe()
      log.info(s"Faucet initialization succeeded?: $walletIsAvailable, retries: $currentRetries")
      (walletIsAvailable, currentRetries) match {
        case (true, _) =>
          context become available
        case (false, retries) if retries < initializationMaxRetries =>
          log.info(s"About to retry initialization in $initializationRetryDelay")
          context become unavailable(retries + 1)
          context.system.scheduler.scheduleOnce(initializationRetryDelay, self, Initialization)
        case _ =>
          log.error(
            s"Couldn't initialize wallet after: $initializationMaxRetries retries (retry delay between them: $initializationRetryDelay)"
          )
      }
    }

    case SendFunds(addressTo: Address) =>
      log.info(
        s"SendFunds called, to: $addressTo, value: ${config.txValue}, gas price: ${config.txGasPrice}," +
          s" gas limit: ${config.txGasLimit} (faucet unavailable)"
      )
      sender() ! FaucetIsUnavailable
  }

  private def available: Receive = {
    case Status =>
      val respondTo = sender()
      respondTo ! StatusResponse(WalletAvailable)

    case Initialization =>
      log.debug("Initialization called (faucet available)")
      sender() ! FaucetIsAlreadyAvailable

    case SendFunds(addressTo: Address) =>
      log.info(
        s"SendFunds called, to: $addressTo, value: ${config.txValue}, gas price: ${config.txGasPrice}, gas limit: ${config.txGasLimit} (faucet available)"
      )
      val respondTo = sender()
      // We Only consider the request fail if we found out
      // wallet is not properly initialized
      walletRpcClient
        .sendFunds(wallet, addressTo)
        .map {
          case Right(txHash) =>
            respondTo ! TransactionSent(txHash)
          case Left(error) =>
            respondTo ! WalletRpcClientError(error.toString)
        }
        .runAsyncAndForget
  }

  private def walletInitialization: Task[Boolean] =
    walletRpcClient.getWallet
      .flatMap {
        case Left(_) => Task.now(false)
        case Right(w) =>
          walletRpcClient.validate(w).map {
            case FaucetStatus.WalletAvailable =>
              wallet = w
              true
            case _ => false
          }
      }
}

object FaucetHandler {

  sealed abstract class FaucetHandlerMsg
  object FaucetHandlerMsg {
    case object Status extends FaucetHandlerMsg
    case object Initialization extends FaucetHandlerMsg
    case class SendFunds(address: Address) extends FaucetHandlerMsg
  }
  sealed trait FaucetHandlerResponse
  object FaucetHandlerResponse {
    case class StatusResponse(status: FaucetStatus) extends FaucetHandlerResponse
    case object FaucetIsUnavailable extends FaucetHandlerResponse
    case object FaucetIsAlreadyAvailable extends FaucetHandlerResponse
    case class WalletRpcClientError(error: String) extends FaucetHandlerResponse
    case class TransactionSent(txHash: ByteString) extends FaucetHandlerResponse
  }

  def props(walletRpcClient: WalletService, config: FaucetConfig): Props = Props(
    new FaucetHandler(walletRpcClient, config)
  )
}
