package io.iohk.ethereum.faucet

import akka.actor.{Actor, ActorLogging, Props}
import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.faucet.FaucetStatus.WalletAvailable
import io.iohk.ethereum.faucet.jsonrpc.WalletService
import io.iohk.ethereum.keystore.Wallet
import monix.execution.Scheduler.Implicits.global

class FaucetHandler(walletService: WalletService, config: FaucetConfig) extends Actor with ActorLogging {

  import FaucetHandler.FaucetHandlerMsg._
  import FaucetHandler.FaucetHandlerResponse._

  var wallet: Wallet = _

  override def preStart(): Unit = {
    self ! Initialization
  }

  override def receive: Receive = unavailable()

  private def unavailable(): Receive = {
    case Status =>
      sender() ! StatusResponse(FaucetStatus.FaucetUnavailable)

    case Initialization => {
      log.info("Initialization called (faucet unavailable)")
      walletService.getWallet.runSyncUnsafe() match {
        case Left(error) =>
          log.error(s"Couldn't initialize wallet - error: $error")
       // context become unavailable
        case Right(w) =>
          log.info("Faucet initialization succeeded")
          wallet = w
          context become available
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
      walletService
        .sendFunds(wallet, addressTo)
        .map {
          case Right(txHash) =>
            respondTo ! TransactionSent(txHash)
          case Left(error) =>
            respondTo ! WalletRpcClientError(error.msg)
        }
        .runAsyncAndForget
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

  val name: String = "FaucetHandler"
}
