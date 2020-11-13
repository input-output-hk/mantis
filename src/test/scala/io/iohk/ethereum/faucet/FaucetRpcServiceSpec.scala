package io.iohk.ethereum.faucet

import java.security.SecureRandom

import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.faucet.jsonrpc.FaucetDomain.{SendFundsRequest, SendFundsResponse}
import io.iohk.ethereum.faucet.jsonrpc.{FaucetRpcService, WalletRpcClient}
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.{crypto, rlp}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class FaucetRpcServiceSpec
    extends AnyFlatSpec
    with Matchers
    with MockFactory
    with ScalatestRouteTest
    with ScalaFutures {

  "FaucetApi" should "send a transaction" in {
    val walletKeyPair = generateKeyPair(new SecureRandom)
    val (prvKey, pubKey) = keyPairToByteStrings(walletKeyPair)
    val wallet = Wallet(Address(crypto.kec256(pubKey)), prvKey)

    val config: FaucetConfig =
      FaucetConfig(wallet.address, "", 10, 20, 1, "", "", 10.seconds)

    val mockWalletRpcClient = mock[WalletRpcClient]
    val mockKeyStore = mock[KeyStore]

    val receivingAddress = Address("0x99")
    val currentNonce = 2

    val tx = wallet.signTx(
      Transaction(currentNonce, config.txGasPrice, config.txGasLimit, receivingAddress, config.txValue, ByteString()),
      None
    )

    val expectedTx = rlp.encode(tx.tx.toRLPEncodable)

    val retTxId = ByteString(Hex.decode("112233"))

    (mockKeyStore.unlockAccount _).expects(config.walletAddress, config.walletPassword).returning(Right(wallet))
    (mockWalletRpcClient.getNonce _).expects(config.walletAddress).returning(Task(Right(currentNonce)))
    (mockWalletRpcClient.sendTransaction _).expects(ByteString(expectedTx)).returning(Task(Right(retTxId)))

    val faucetRpcService = new FaucetRpcService(mockWalletRpcClient, mockKeyStore, config)

    val res = faucetRpcService.sendFunds(SendFundsRequest(Address("0x99"))).runSyncUnsafe()

    res shouldEqual Right(SendFundsResponse(retTxId))
  }

}
