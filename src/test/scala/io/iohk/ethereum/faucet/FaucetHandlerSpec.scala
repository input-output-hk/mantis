package io.iohk.ethereum.faucet

import java.security.SecureRandom

import akka.actor.ActorSystem
import akka.pattern.gracefulStop
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.ByteString
import io.iohk.ethereum.crypto.{generateKeyPair, keyPairToByteStrings}
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.{NormalPatience, WithActorSystemShutDown, crypto}
import io.iohk.ethereum.faucet.FaucetHandler.{FaucetHandlerMsg, FaucetHandlerResponse}
import io.iohk.ethereum.faucet.jsonrpc.WalletService
import io.iohk.ethereum.keystore.KeyStore.DecryptionFailed
import io.iohk.ethereum.keystore.Wallet
import io.iohk.ethereum.mallet.common.{ParserError, RpcClientError}
import monix.eval.Task
import org.bouncycastle.util.encoders.Hex
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

      "should try to unlock the Wallet if it is not initialized" in new TestSetup {
        withUnavailableFaucet {
          faucetHandler ! FaucetHandlerMsg.Initialization

          sender.send(faucetHandler, FaucetHandlerMsg.Status)
          sender.expectMsg(FaucetHandlerResponse.StatusResponse(FaucetStatus.FaucetUnavailable))
        }
      }

      "shouldn't send funds if the Faucet isn't initialized" in new TestSetup {
         withUnavailableFaucet {
           sender.send(faucetHandler, FaucetHandlerMsg.SendFunds(paymentAddress))
           sender.expectMsg(FaucetHandlerResponse.FaucetIsUnavailable)
         }
       }

      "should respond that it is available if it was initialized successfully" in new TestSetup {
         withInitializedFaucet {
           sender.send(faucetHandler, FaucetHandlerMsg.Initialization)
           sender.expectMsg(FaucetHandlerResponse.FaucetIsAlreadyAvailable)
         }
       }

      "should respond that it is available when ask the status if it was initialized successfully" in new TestSetup {
        withInitializedFaucet {
          sender.send(faucetHandler, FaucetHandlerMsg.Status)
          sender.expectMsg(FaucetHandlerResponse.StatusResponse(FaucetStatus.WalletAvailable))
        }
      }

      "should be able to paid if it was initialized successfully" in new TestSetup {
        withInitializedFaucet {
          val retTxId = ByteString(Hex.decode("112233"))
          (walletService.sendFunds _).expects(wallet, paymentAddress).returning(Task.pure(Right(retTxId)))

          sender.send(faucetHandler, FaucetHandlerMsg.SendFunds(paymentAddress))
          sender.expectMsg(FaucetHandlerResponse.TransactionSent(retTxId))
        }
      }

      "should failed the payment if dont can parse the payload" in new TestSetup {
        withInitializedFaucet {
          val errorMessage = "parser error"
          (walletService.sendFunds _).expects(wallet, paymentAddress).returning(Task.pure(Left(ParserError(errorMessage))))

          sender.send(faucetHandler, FaucetHandlerMsg.SendFunds(paymentAddress))
          sender.expectMsg(FaucetHandlerResponse.WalletRpcClientError(errorMessage))
        }
      }

      "should failed the payment if throw rpc client error" in new TestSetup {
        withInitializedFaucet {
          val errorMessage = "client timeout"
          (walletService.sendFunds _).expects(wallet, paymentAddress).returning(Task.pure(Left(RpcClientError(errorMessage))))

          sender.send(faucetHandler, FaucetHandlerMsg.SendFunds(paymentAddress))
          sender.expectMsg(FaucetHandlerResponse.WalletRpcClientError(errorMessage))
        }
      }
    }
  }

  implicit val ec: ExecutionContext = ExecutionContext.global

  trait TestSetup extends MockFactory with FaucetConfigBuilder{

    val walletService: WalletService = mock[WalletService]
    val paymentAddress: Address = Address("0x99")

    val faucetHandler = system.actorOf(FaucetHandler.props(walletService, faucetConfig))

    val walletKeyPair = generateKeyPair(new SecureRandom)
    val (prvKey, pubKey) = keyPairToByteStrings(walletKeyPair)
    val wallet = Wallet(Address(crypto.kec256(pubKey)), prvKey)

    val sender = TestProbe()

    def withUnavailableFaucet(behaviour: => Unit): Unit = {
      (walletService.getWallet _).expects().returning(Task.pure(Left(DecryptionFailed)))

      sender.send(faucetHandler, FaucetHandlerMsg.Status)
      sender.expectMsg(FaucetHandlerResponse.StatusResponse(FaucetStatus.FaucetUnavailable))

      behaviour
      stopController()
    }

    def withInitializedFaucet(behaviour: => Unit): Unit = {
      (walletService.getWallet _).expects().returning(Task.pure(Right(wallet)))

      faucetHandler ! FaucetHandlerMsg.Initialization

      sender.send(faucetHandler, FaucetHandlerMsg.Status)
      sender.expectMsg(FaucetHandlerResponse.StatusResponse(FaucetStatus.WalletAvailable))

      behaviour
      stopController()
    }

    def stopController(): Unit = {
      awaitCond(gracefulStop(faucetHandler, actorAskTimeout.duration).futureValue)
    }
  }
}
