package io.iohk.ethereum.faucet.jsonrpc

import java.security.SecureRandom

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.faucet.{FaucetConfig, RpcClientConfig, SupervisorConfig}
import io.iohk.ethereum.jsonrpc.client.RpcClient.ConnectionError
import io.iohk.ethereum.keystore.KeyStore.DecryptionFailed
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.network.p2p.messages.PV60.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.{crypto, rlp}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class WalletServiceSpec extends AnyFlatSpec with Matchers with MockFactory {

  "Wallet Service" should "send a transaction successfully when getNonce and sendTransaction successfully" in new TestSetup {

    val receivingAddress = Address("0x99")
    val currentNonce = 2

    val tx = wallet.signTx(
      Transaction(currentNonce, config.txGasPrice, config.txGasLimit, receivingAddress, config.txValue, ByteString()),
      None
    )

    val expectedTx = rlp.encode(tx.tx.toRLPEncodable)

    val retTxId = ByteString(Hex.decode("112233"))

    (walletRpcClient.getNonce _).expects(config.walletAddress).returning(Task.pure(Right(currentNonce)))
    (walletRpcClient.sendTransaction _).expects(ByteString(expectedTx)).returning(Task.pure(Right(retTxId)))

    val res = walletService.sendFunds(wallet, Address("0x99")).runSyncUnsafe()

    res shouldEqual Right(retTxId)

  }

  it should "failure the transaction when get timeout of getNonce" in new TestSetup {

    val timeout = ConnectionError("timeout")
    (walletRpcClient.getNonce _).expects(config.walletAddress).returning(Task.pure(Left(timeout)))

    val res = walletService.sendFunds(wallet, Address("0x99")).runSyncUnsafe()

    res shouldEqual Left(timeout)

  }

  it should "get wallet successful" in new TestSetup {
    (mockKeyStore.unlockAccount _).expects(config.walletAddress, config.walletPassword).returning(Right(wallet))

    val res = walletService.getWallet.runSyncUnsafe()

    res shouldEqual Right(wallet)
  }

  it should "wallet decryption failed" in new TestSetup {
    (mockKeyStore.unlockAccount _)
      .expects(config.walletAddress, config.walletPassword)
      .returning(Left(DecryptionFailed))

    val res = walletService.getWallet.runSyncUnsafe()

    res shouldEqual Left(DecryptionFailed)
  }

  trait TestSetup {
    val walletKeyPair = generateKeyPair(new SecureRandom)
    val (prvKey, pubKey) = keyPairToByteStrings(walletKeyPair)
    val wallet = Wallet(Address(crypto.kec256(pubKey)), prvKey)

    val walletRpcClient = mock[WalletRpcClient]
    val mockKeyStore = mock[KeyStore]
    val config: FaucetConfig =
      FaucetConfig(
        walletAddress = wallet.address,
        walletPassword = "",
        txGasPrice = 10,
        txGasLimit = 20,
        txValue = 1,
        rpcClient = RpcClientConfig("", timeout = 10.seconds),
        keyStoreDir = "",
        handlerTimeout = 10.seconds,
        actorCommunicationMargin = 10.seconds,
        supervisor = mock[SupervisorConfig],
        shutdownTimeout = 15.seconds
      )

    val walletService = new WalletService(walletRpcClient, mockKeyStore, config)
  }

}
