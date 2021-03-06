package io.iohk.ethereum.network

import java.math.BigInteger
import java.net.URI

import akka.util.ByteString

import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import org.bouncycastle.crypto.params.ECPrivateKeyParameters
import org.bouncycastle.crypto.params.ECPublicKeyParameters
import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.rlpx.AuthHandshakeSuccess
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.AuthResponseMessage
import io.iohk.ethereum.network.rlpx.Secrets
import io.iohk.ethereum.security.SecureRandomBuilder

class AuthHandshakerSpec extends AnyFlatSpec with Matchers with SecureRandomBuilder {

  val remoteNodeKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(
      curve.getCurve.decodePoint(
        Hex.decode(
          "0491376c89ba75cc51fd6b63af01083e6cc11f5635620527e254a03374738e1eb344b2221470a4638e670a97a06f3b91c4f517ccc325561148b106407671d5c46d"
        )
      ),
      curve
    ),
    new ECPrivateKeyParameters(
      new BigInteger("105751695959748236927330749967459856049816015502376529986938855740081063876828"),
      curve
    )
  )

  val remoteEphemeralKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(
      curve.getCurve.decodePoint(
        Hex.decode(
          "0404753378699da7678151e9a0605aa0b07ba4f31764b624c90d497d0c78b56f8fe47cd78e1eef35022d1241c7d2ee42eac74a9036f3ed0b8027ce484b556e789e"
        )
      ),
      curve
    ),
    new ECPrivateKeyParameters(
      new BigInteger("92053546780651665949308997856114509339625788837073204328320628931366416758609"),
      curve
    )
  )

  val remoteNonce: ByteString = ByteString(Array.fill[Byte](AuthHandshaker.NonceSize)(9.toByte))

  val remoteNodeId: Array[Byte] = remoteNodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId
  val remoteUri = new URI(s"enode://${Hex.toHexString(remoteNodeId)}@127.0.0.1:30303")

  val nodeKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(
      curve.getCurve.decodePoint(
        Hex.decode(
          "045a57761ca5e81288f32b4136e5a8f8d816a0b992b6dfea75312d2dd7618ee8f7e113aaa732dd77f901a7af43275280b985b9f539615733cdf7fbe06636813d4b"
        )
      ),
      curve
    ),
    new ECPrivateKeyParameters(
      new BigInteger("51209471710014748445103304012335548896378228839026325233666834803269084805514"),
      curve
    )
  )

  val ephemeralKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(
      curve.getCurve.decodePoint(
        Hex.decode(
          "04ead31caeaf59d1299991c16910f68cd61216a67e397111429d2800f58e849940fc0bf8c8f1df05c7de40cd21a2b0bed9d0c3c184034f9d5fd54c4476ddd8d6ed"
        )
      ),
      curve
    ),
    new ECPrivateKeyParameters(
      new BigInteger("47209959662887443680833530073996538660770112643177512357678065781331682025297"),
      curve
    )
  )

  val nonce: ByteString = ByteString(Array.fill[Byte](AuthHandshaker.NonceSize)(1.toByte))

  "AuthHandshaker" should "handle init response" in {
    val (_, authHandshaker) = AuthHandshaker(nodeKey, nonce, ephemeralKey, secureRandom).initiate(remoteUri)

    val response = AuthResponseMessage(
      ephemeralPublicKey = remoteEphemeralKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ,
      nonce = remoteNonce,
      knownPeer = false
    )

    val encodedResponse = response.encoded
    val encryptedResponse = ECIESCoder.encrypt(
      nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ,
      secureRandom,
      encodedResponse.toArray
    )

    val AuthHandshakeSuccess(secrets: Secrets, _) = authHandshaker.handleResponseMessage(ByteString(encryptedResponse))

    val expectedMacSecret = Hex.decode("50a782c6fedf88b829a6e5798da721dcbf5b46c117704e2ada985d5235ac192c")
    val expectedSharedToken = Hex.decode("b1960fa5d529ee89f8032c8aeb0e4fda2bbf4d7eff0c5695173e27f382d8f5bb")
    val expectedAesSecret = Hex.decode("55e7896a728e74650b3da1e4011823983551d1b5a5bfaf166627da9bea25a562")

    secrets.mac shouldBe expectedMacSecret
    secrets.token shouldBe expectedSharedToken
    secrets.aes shouldBe expectedAesSecret
  }

  it should "handle both incoming packet and a response" in {
    val thisHandshaker = AuthHandshaker(nodeKey, nonce, ephemeralKey, secureRandom)
    val remoteHandshaker = AuthHandshaker(remoteNodeKey, remoteNonce, remoteEphemeralKey, secureRandom)

    val (initPacket, thisHandshakerInitiated) = thisHandshaker.initiate(remoteUri)
    val (responsePacket, AuthHandshakeSuccess(remoteSecrets: Secrets, _)) =
      remoteHandshaker.handleInitialMessageV4(initPacket)
    val AuthHandshakeSuccess(thisSecrets: Secrets, _) = thisHandshakerInitiated.handleResponseMessageV4(responsePacket)

    remoteSecrets.token shouldBe thisSecrets.token
    remoteSecrets.aes shouldBe thisSecrets.aes
    remoteSecrets.mac shouldBe thisSecrets.mac
  }

}
