package io.iohk.ethereum.faucet.jsonrpc

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.faucet.FaucetHandler.FaucetHandlerMsg
import io.iohk.ethereum.faucet.FaucetHandler.FaucetHandlerResponse.{
  FaucetIsUnavailable,
  StatusResponse,
  TransactionSent,
  WalletRpcClientError
}
import io.iohk.ethereum.faucet.FaucetStatus.WalletAvailable
import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.{SendFundsRequest, StatusRequest}
import io.iohk.ethereum.faucet.{FaucetConfig, SupervisorConfig}
import io.iohk.ethereum.jsonrpc.JsonRpcError
import io.iohk.ethereum.testing.ActorsTesting.simpleAutoPilot
import io.iohk.ethereum.{NormalPatience, WithActorSystemShutDown}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.bouncycastle.util.encoders.Hex
import org.scalactic.TypeCheckedTripleEquals
import org.scalamock.scalatest.MockFactory
import org.scalatest.OptionValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class FaucetRpcServiceSpec
    extends TestKit(ActorSystem("ActorSystem_DebugFaucetRpcServiceSpec"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers
    with ScalaFutures
    with OptionValues
    with MockFactory
    with NormalPatience
    with TypeCheckedTripleEquals {

  "FaucetRpcService" should "answer txHash correctly when the wallet is available and the requested send funds be successfully" in new TestSetup {
    val address: Address = Address("0x00")
    val request: SendFundsRequest = SendFundsRequest(address)
    val txHash: ByteString = ByteString(Hex.decode("112233"))

    fHandler.setAutoPilot(simpleAutoPilot { case FaucetHandlerMsg.SendFunds(`address`) =>
      TransactionSent(txHash)
    })
    faucetRpcService.sendFunds(request).runSyncUnsafe(Duration.Inf) match {
      case Left(error) => fail(s"failure with error: $error")
      case Right(response) => response.txId shouldBe txHash
    }
  }

  it should "answer WalletRpcClientError when the wallet is available and the requested send funds be failure" in new TestSetup {
    val address: Address = Address("0x00")
    val request: SendFundsRequest = SendFundsRequest(address)
    val clientError: String = "Parser error"

    fHandler.setAutoPilot(simpleAutoPilot { case FaucetHandlerMsg.SendFunds(`address`) =>
      WalletRpcClientError(clientError)
    })
    faucetRpcService.sendFunds(request).runSyncUnsafe(Duration.Inf) match {
      case Right(_) => fail()
      case Left(error) => error shouldBe JsonRpcError.LogicError(s"Faucet error: $clientError")
    }
  }

  it should "answer FaucetIsUnavailable when tried to send funds and the wallet is unavailable" in new TestSetup {
    val address: Address = Address("0x00")
    val request: SendFundsRequest = SendFundsRequest(address)

    fHandler.setAutoPilot(simpleAutoPilot { case FaucetHandlerMsg.SendFunds(`address`) =>
      FaucetIsUnavailable
    })
    faucetRpcService.sendFunds(request).runSyncUnsafe(Duration.Inf) match {
      case Right(_) => fail()
      case Left(error) =>
        error shouldBe JsonRpcError.LogicError("Faucet is unavailable: Please try again in a few more seconds")
    }
  }

  it should "answer FaucetIsUnavailable when tried to get status and the wallet is unavailable" in new TestSetup {
    fHandler.setAutoPilot(simpleAutoPilot { case FaucetHandlerMsg.Status =>
      FaucetIsUnavailable
    })
    faucetRpcService.status(StatusRequest()).runSyncUnsafe(Duration.Inf) match {
      case Right(_) => fail()
      case Left(error) =>
        error shouldBe JsonRpcError.LogicError("Faucet is unavailable: Please try again in a few more seconds")
    }
  }

  it should "answer WalletAvailable when tried to get status and the wallet is available" in new TestSetup {
    fHandler.setAutoPilot(simpleAutoPilot { case FaucetHandlerMsg.Status =>
      StatusResponse(WalletAvailable)
    })
    faucetRpcService.status(StatusRequest()).runSyncUnsafe(Duration.Inf) match {
      case Left(error) => fail(s"failure with error: $error")
      case Right(response) => response shouldBe FaucetDomain.StatusResponse(WalletAvailable)
    }
  }

  it should "answer internal error when tried to send funds but the Faucet Handler is disable" in new TestSetup {
    val address: Address = Address("0x00")
    val request: SendFundsRequest = SendFundsRequest(address)

    faucetRpcServiceWithoutFaucetHandler.sendFunds(request).runSyncUnsafe(Duration.Inf) match {
      case Right(_) => fail()
      case Left(error) =>
        error shouldBe JsonRpcError.InternalError
    }
  }

  it should "answer internal error when tried to get status but the Faucet Handler is disable" in new TestSetup {
    val address: Address = Address("0x00")
    val request: SendFundsRequest = SendFundsRequest(address)

    faucetRpcServiceWithoutFaucetHandler.status(StatusRequest()).runSyncUnsafe(Duration.Inf) match {
      case Right(_) => fail()
      case Left(error) =>
        error shouldBe JsonRpcError.InternalError
    }
  }

  class TestSetup(implicit system: ActorSystem) {

    val config: FaucetConfig = FaucetConfig(
      walletAddress = Address("0x99"),
      walletPassword = "",
      txGasPrice = 10,
      txGasLimit = 20,
      txValue = 1,
      rpcAddress = "",
      keyStoreDir = "",
      minRequestInterval = 10.seconds,
      handlerTimeout = 10.seconds,
      responseTimeout = 10.seconds,
      supervisor = mock[SupervisorConfig],
      shutdownTimeout = 15.seconds
    )

    val fHandler = TestProbe()

    val faucetRpcService: FaucetRpcService = new FaucetRpcService(config) {

      override def faucetHandler()(implicit system: ActorSystem): Task[ActorRef] = {
        Task(fHandler.ref)
      }
    }

    val faucetRpcServiceWithoutFaucetHandler: FaucetRpcService = new FaucetRpcService(config) {
      override def faucetHandler()(implicit system: ActorSystem): Task[ActorRef] = {
        Task.raiseError(new RuntimeException("time out"))
      }
    }
  }

}
