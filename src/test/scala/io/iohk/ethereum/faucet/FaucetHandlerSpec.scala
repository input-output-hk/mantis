package io.iohk.ethereum.faucet

import akka.actor.ActorSystem
import akka.pattern.gracefulStop
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import io.iohk.ethereum.{NormalPatience, WithActorSystemShutDown}
import io.iohk.ethereum.faucet.FaucetHandler.{FaucetHandlerMsg, FaucetHandlerResponse}
import io.iohk.ethereum.faucet.jsonrpc.WalletService
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

class FaucetHandlerSpec extends TestKit(ActorSystem("ActorSystem_DebugFaucetHandlerSpec"))
  with AnyFreeSpecLike
  with ImplicitSender
  with WithActorSystemShutDown
  with Matchers
  with MockFactory
  with ScalaFutures
  with NormalPatience {

  "Faucet Handler" - {
    "initialization" - {

      "should not return ommers if there is no any" in new TestSetup {
      }
    }
  }

  implicit val ec: ExecutionContext = ExecutionContext.global

  trait TestSetup extends MockFactory with FaucetConfigBuilder{

    lazy val walletService: WalletService = mock[WalletService]

   // (walletService.getWallet _).expects().returning(Right(List(wallet.address)))
   // (config.initializationRetryDelay _).expects().returning(Right(List(wallet.address)))
    lazy val faucetHandler = system.actorOf(FaucetHandler.props(walletService, faucetConfig))

    lazy val sender = TestProbe()

    def withUnavailableFaucet(behaviour: => Unit): Unit = {
      sender.send(faucetHandler, FaucetHandlerMsg.Status)
      sender.expectMsg(FaucetHandlerResponse.StatusResponse(FaucetStatus.FaucetUnavailable))
      behaviour
      stopController()
    }

    def stopController(): Unit = {
      awaitCond(gracefulStop(faucetHandler, actorAskTimeout.duration).futureValue)
    }
  }
}
