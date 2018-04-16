package io.iohk.ethereum.faucet

import java.security.SecureRandom

import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.HttpOriginRange
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.crypto._
import org.scalatest.{FlatSpec, Matchers}
import org.scalamock.scalatest.MockFactory
import io.iohk.ethereum.rlp
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import org.spongycastle.util.encoders.Hex

class FaucetApiSpec extends FlatSpec with Matchers with MockFactory with ScalatestRouteTest {

  "FaucetApi" should "send a transaction" in {
    val walletKeyPair = generateKeyPair(new SecureRandom)
    val (prvKey, pubKey) = keyPairToByteStrings(walletKeyPair)
    val wallet = Wallet(Address(crypto.kec256(pubKey)), prvKey)

    val config = FaucetConfig(wallet.address, "", 10, 20, 1, HttpOriginRange.*, "", "", "", 0)

    val mockRpcClient = mock[RpcClient]
    val mockKeyStore = mock[KeyStore]

    val receivingAddress = Address("0x99")
    val currentNonce = 2

    val expectedTx = rlp.encode(wallet.signTx(Transaction(currentNonce + 1, config.txGasPrice, config.txGasLimit, receivingAddress,
    config.txValue, ByteString()), None).toRLPEncodable)

    val retTxId = ByteString(Hex.decode("112233"))

    (mockKeyStore.unlockAccount _).expects(config.walletAddress, config.walletPassword).returning(Right(wallet))
    (mockRpcClient.getNonce _).expects(config.walletAddress).returning(Right(currentNonce))
    (mockRpcClient.sendTransaction _).expects(ByteString(expectedTx)).returning(Right(retTxId))

    val faucetApi = new FaucetApi(mockRpcClient, mockKeyStore, config)

    val postRequest = HttpRequest(HttpMethods.POST, uri = "/faucet?address=0x99")

    postRequest ~>  Route.seal(faucetApi.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual "0x112233"
    }
  }

}
