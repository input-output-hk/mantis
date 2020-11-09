package io.iohk.ethereum.faucet

import java.security.SecureRandom

import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.iohk.ethereum.crypto
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.keystore.Wallet
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WalletServiceSpec extends AnyFlatSpec with Matchers with MockFactory with ScalatestRouteTest with ScalaFutures {

  "FaucetApi" should "send a transaction" in {
    val walletKeyPair = generateKeyPair(new SecureRandom)
    val (prvKey, pubKey) = keyPairToByteStrings(walletKeyPair)
    val wallet = Wallet(Address(crypto.kec256(pubKey)), prvKey)

    /* val config: FaucetConfig =
      FaucetConfig(wallet.address, "", 10, 20, 1, "", "", 10.seconds)

    val mockRpcClient = mock[RpcClient]
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
    (mockRpcClient.getNonce _).expects(config.walletAddress).returning(Right(currentNonce))
    (mockRpcClient.sendTransaction _).expects(ByteString(expectedTx)).returning(Right(retTxId))

    val faucetRpcService = new FaucetRpcService(mockRpcClient, mockKeyStore, config)

    val res = faucetRpcService.sendFunds(SendFundsRequest(Address("0x99"))).runSyncUnsafe()

    res shouldEqual Right(SendFundsResponse(retTxId))*/
  }

}
