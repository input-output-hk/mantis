package io.iohk.ethereum.faucet

import java.security.SecureRandom

import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import ch.megard.akka.http.cors.scaladsl.model.HttpOriginMatcher
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.domain.{Address, Transaction}
import io.iohk.ethereum.faucet.jsonrpc.FaucetRpcService
import io.iohk.ethereum.keystore.{KeyStore, Wallet}
import io.iohk.ethereum.mallet.service.RpcClient
import io.iohk.ethereum.network.p2p.messages.CommonMessages.SignedTransactions.SignedTransactionEnc
import io.iohk.ethereum.{crypto, rlp}
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class FaucetRpcServicepec extends AnyFlatSpec with Matchers with MockFactory with ScalatestRouteTest {

  "FaucetApi" should "send a transaction" in {
    val walletKeyPair = generateKeyPair(new SecureRandom)
    val (prvKey, pubKey) = keyPairToByteStrings(walletKeyPair)
    val wallet = Wallet(Address(crypto.kec256(pubKey)), prvKey)

    val config = FaucetConfig(wallet.address, "", 10, 20, 1, HttpOriginMatcher.*, "", "", "", 0, 10.seconds, 1024)

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

    //TODO...

    /*val postRequest = HttpRequest(HttpMethods.POST, uri = "/faucet?address=0x99")

    postRequest ~> Route.seal(faucetApi.route) ~> check {
      status shouldEqual StatusCodes.OK
      responseAs[String] shouldEqual "0x112233"
    }*/
  }

  /*it should "limit the number of requests from single ip address" in {
    val walletKeyPair = generateKeyPair(new SecureRandom)
    val (prvKey, pubKey) = keyPairToByteStrings(walletKeyPair)
    val wallet = Wallet(Address(crypto.kec256(pubKey)), prvKey)

    val config = FaucetConfig(wallet.address, "", 10, 20, 1, HttpOriginMatcher.*, "", "", "", 0, 10.seconds, 1024)

    val mockRpcClient = mock[RpcClient]
    val mockKeyStore = mock[KeyStore]

    val retTxId = ByteString(Hex.decode(""))

    (mockKeyStore.unlockAccount _).expects(config.walletAddress, config.walletPassword).returning(Right(wallet))
    (mockRpcClient.getNonce _).expects(config.walletAddress).returning(Right(1))
    (mockRpcClient.sendTransaction _).expects(*).returning(Right(retTxId))

    val mockClock = mock[Clock]
    val faucetApi = new FaucetApi(mockRpcClient, mockKeyStore, config, mockClock)

    val baseReq = HttpRequest(HttpMethods.POST, uri = "/faucet?address=0x99")

    val postRequestFromClient1 =
      baseReq.withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getByName("127.0.0.1"))))

    val postRequestFromClient2 =
      baseReq.withHeaders(`X-Forwarded-For`(RemoteAddress(InetAddress.getByName("127.0.0.2"))))

    (mockClock.instant _).expects().returning(Instant.ofEpochMilli(15000000))
    postRequestFromClient1 ~> Route.seal(faucetApi.route) ~> check {
      status shouldEqual StatusCodes.OK
    }

    (mockClock.instant _).expects().returning(Instant.ofEpochMilli(15000001))
    postRequestFromClient1 ~> Route.seal(faucetApi.route) ~> check {
      status shouldEqual StatusCodes.TooManyRequests
    }

    (mockClock.instant _).expects().returning(Instant.ofEpochMilli(15000002))
    (mockRpcClient.getNonce _).expects(config.walletAddress).returning(Right(1))
    (mockRpcClient.sendTransaction _).expects(*).returning(Right(retTxId))

    postRequestFromClient2 ~> Route.seal(faucetApi.route) ~> check {
      status shouldEqual StatusCodes.OK
    }

    (mockClock.instant _).expects().returning(Instant.ofEpochMilli(15011001))
    (mockRpcClient.getNonce _).expects(config.walletAddress).returning(Right(1))
    (mockRpcClient.sendTransaction _).expects(*).returning(Right(retTxId))

    postRequestFromClient1 ~> Route.seal(faucetApi.route) ~> check {
      status shouldEqual StatusCodes.OK
    }

  }*/

}
