package io.iohk.ethereum.faucet

import monix.execution.Scheduler.Implicits.global
import akka.actor.{Actor, ActorLogging, Props}
import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.faucet.jsonrpc.WalletRpcClient
import monix.eval.Task

class FaucetHandler(walletRpcClient: WalletRpcClient, config: FaucetConfig) extends Actor with ActorLogging {

  private val initializationRetryDelay = config.initializationRetryDelay
  private val initializationMaxRetries = config.initializationMaxRetries

  import FaucetHandler.FaucetHandlerMsg._
  import FaucetHandler.FaucetHandlerResponse._

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
        case (false, retries) if (retries < initializationMaxRetries) =>
          log.info(s"About to retry initialization in $initializationRetryDelay")
          context become unavailable(retries + 1)
          context.system.scheduler.scheduleOnce(initializationRetryDelay, self, Initialization)
        case _ =>
          log.error(s"Couldn't initialize wallet after: $initializationMaxRetries retries (retry delay between them: $initializationRetryDelay)")
      }
    }

    case SendFunds(addressTo: Address) =>
      log.info(s"SendFunds called, to: $addressTo, value: ${config.txValue}, gas price: ${config.txGasPrice}," +
        s" gas limit: ${config.txGasLimit} (faucet unavailable)")
      self ! Initialization //TODO: in progress
      //sender() ! FaucetIsUnavailable

    case Unavailable =>
      log.debug("Unavailable called (faucet unavailable)")
  }

  private def available: Receive = {
    case Status =>
      val respondTo = sender()
      walletRpcClient.getWalletStatus.flatMap { response =>
        Task { respondTo ! StatusResponse(response) }
      }.runAsyncAndForget

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
        .sendFunds(addressTo)
        .map {
          case Right(txHash) =>
            respondTo ! TransactionSent(txHash)
          case Left(error) =>
            respondTo ! WalletRpcClientError(error.toString)
          //TODO...
          /*case Left(error) =>
            error match {
              case SendFundsError.WalletNotInitialized =>
                respondTo ! WalletRpcClientError(error.toString)
                self ! Unavailable
              case SendFundsError.WalletError(_) =>
                respondTo ! WalletRpcClientError(error.toString)
            }*/
        }
        .runAsyncAndForget

    case Unavailable =>
      log.info("Faucet become unavailable")
      context become unavailable()
      self ! Initialization
  }

  private def walletInitialization: Task[Boolean] =
    walletRpcClient.getWalletStatus
      .flatMap {
        case FaucetStatus.WalletDoesNotExist =>
          for {
            _ <- walletRpcClient.restoreWallet()
            isUnlocked <- walletRpcClient.unlockWallet()
          } yield isUnlocked match {
            case Right(value) => value
            case Left(_) => false
          }
        case FaucetStatus.WalletIsUnlocked =>
          Task.pure(true)
        case FaucetStatus.WalletNotResponds =>
          Task.pure(false)
      }
}

object FaucetHandler {

  sealed abstract class FaucetHandlerMsg
  object FaucetHandlerMsg {
    case object Status extends FaucetHandlerMsg
    case object Initialization extends FaucetHandlerMsg
    case object Unavailable extends FaucetHandlerMsg
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

  def props(walletRpcClient: WalletRpcClient, config: FaucetConfig): Props = Props(new FaucetHandler(walletRpcClient, config))
}
