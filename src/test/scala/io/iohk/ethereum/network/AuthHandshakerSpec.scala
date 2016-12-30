package io.iohk.ethereum.network

import java.math.BigInteger
import java.net.URI

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.util.encoders.Hex
import scorex.core.network.AuthHandshakeSuccess

class AuthHandshakerSpec extends FlatSpec with Matchers {

  val remoteNodeKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(curve.getCurve.decodePoint(Hex.decode("0491376c89ba75cc51fd6b63af01083e6cc11f5635620527e254a03374738e1eb344b2221470a4638e670a97a06f3b91c4f517ccc325561148b106407671d5c46d")), curve),
    new ECPrivateKeyParameters(new BigInteger("105751695959748236927330749967459856049816015502376529986938855740081063876828"), curve))

  val remoteEphemeralKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(curve.getCurve.decodePoint(Hex.decode("0404753378699da7678151e9a0605aa0b07ba4f31764b624c90d497d0c78b56f8fe47cd78e1eef35022d1241c7d2ee42eac74a9036f3ed0b8027ce484b556e789e")), curve),
    new ECPrivateKeyParameters(new BigInteger("92053546780651665949308997856114509339625788837073204328320628931366416758609"), curve))

  val remoteNonce = ByteString(Array.fill[Byte](AuthHandshaker.nonceSize)(9.toByte))

  val remoteNodeId = nodeIdFromPublicKey(remoteNodeKey.getPublic)
  val remoteUri = new URI(s"enode://${Hex.toHexString(remoteNodeId)}@127.0.0.1:30303")

  val nodeKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(curve.getCurve.decodePoint(Hex.decode("045a57761ca5e81288f32b4136e5a8f8d816a0b992b6dfea75312d2dd7618ee8f7e113aaa732dd77f901a7af43275280b985b9f539615733cdf7fbe06636813d4b")), curve),
    new ECPrivateKeyParameters(new BigInteger("51209471710014748445103304012335548896378228839026325233666834803269084805514"), curve))

  val ephemeralKey = new AsymmetricCipherKeyPair(
    new ECPublicKeyParameters(curve.getCurve.decodePoint(Hex.decode("04ead31caeaf59d1299991c16910f68cd61216a67e397111429d2800f58e849940fc0bf8c8f1df05c7de40cd21a2b0bed9d0c3c184034f9d5fd54c4476ddd8d6ed")), curve),
    new ECPrivateKeyParameters(new BigInteger("47209959662887443680833530073996538660770112643177512357678065781331682025297"), curve))

  val nonce = ByteString(Array.fill[Byte](AuthHandshaker.nonceSize)(1.toByte))

  "AuthHandshaker" should "create init packet" in {
    val authHandshaker = AuthHandshaker(nodeKey, nonce, ephemeralKey)
    val (initPacket, _) = authHandshaker.initiate(remoteUri)
    val decryptedPacket = ECIESCoder.decrypt(remoteNodeKey.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD, initPacket.toArray)

    val expectedDecryptedPacket = Hex.decode("246ab22d722132310385be6cc3f539b0ed1409148edd6546cc714567047ca29c4335b9e3ce9de9622fb374e5439e40cfe97484ec2630d4ac1e7ac097a5dcb5dd010f188d07681e8d98a17ce549645ff4ca1aa6a85b09e50666d31cec8fea1462635a57761ca5e81288f32b4136e5a8f8d816a0b992b6dfea75312d2dd7618ee8f7e113aaa732dd77f901a7af43275280b985b9f539615733cdf7fbe06636813d4b010101010101010101010101010101010101010101010101010101010101010100")

    decryptedPacket shouldBe expectedDecryptedPacket
  }

  it should "handle init response" in {
    val (_, authHandshaker) = AuthHandshaker(nodeKey, nonce, ephemeralKey).initiate(remoteUri)

    val response = AuthResponseMessage(
      ephemeralPublicKey = remoteEphemeralKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ,
      nonce = remoteNonce,
      knownPeer = false)

    val encodedResponse = response.encode()
    val encryptedResponse = ECIESCoder.encrypt(nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ, encodedResponse.toArray)

    val AuthHandshakeSuccess(secrets: Secrets) = authHandshaker.handleResponseMessage(ByteString(encryptedResponse))

    val expectedMacSecret =  Hex.decode("50a782c6fedf88b829a6e5798da721dcbf5b46c117704e2ada985d5235ac192c")
    val expectedSharedToken =  Hex.decode("b1960fa5d529ee89f8032c8aeb0e4fda2bbf4d7eff0c5695173e27f382d8f5bb")
    val expectedAesSecret = Hex.decode("55e7896a728e74650b3da1e4011823983551d1b5a5bfaf166627da9bea25a562")

    secrets.mac shouldBe expectedMacSecret
    secrets.token shouldBe expectedSharedToken
    secrets.aes shouldBe expectedAesSecret
  }

  it should "handle both incoming packet and a response" in {
    val thisHandshaker = AuthHandshaker(nodeKey, nonce, ephemeralKey)
    val remoteHandshaker = AuthHandshaker(remoteNodeKey, remoteNonce, remoteEphemeralKey)

    val (initPacket, thisHandshakerInitiated) = thisHandshaker.initiate(remoteUri)
    val (responsePacket, AuthHandshakeSuccess(remoteSecrets: Secrets)) = remoteHandshaker.handleInitialMessage(initPacket)
    val AuthHandshakeSuccess(thisSecrets: Secrets) = thisHandshakerInitiated.handleResponseMessage(responsePacket)

    remoteSecrets.token shouldBe thisSecrets.token
    remoteSecrets.aes shouldBe thisSecrets.aes
    remoteSecrets.mac shouldBe thisSecrets.mac
  }

}
