package io.iohk.ethereum.network

import java.security.SecureRandom

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECIESCoder
import io.iohk.ethereum.crypto._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.agreement.ECDHBasicAgreement
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.generators.ECKeyPairGenerator
import org.spongycastle.crypto.params._
import org.spongycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}

import scala.util.Random

object Handshaker {

  val nodeKey = generateKeyPair()

  var sessionTokenStorage = Map.empty[String, Array[Byte]]

  def handshake(host: String, port: Int, remoteNodeId: String) = {
    val remotePubKey = publicKeyFromNodeId(remoteNodeId)

    val authInitiateMessage = createAuthInitiateMessage(remoteNodeId)

    val initiatePacket = ECIESCoder.encrypt(remotePubKey, authInitiateMessage.encode(), None)

    // send initiatePacket and wait for response

    // if (!sessionTokenStorage.contains(remoteNodeId))
    //   sessionTokenStorage += (remoteNodeId -> authInitiateMessage)
  }

  def createAuthInitiateMessage(remoteNodeId: String) = {
    val remotePubKey = publicKeyFromNodeId(remoteNodeId)

    val ephemeralKey = generateKeyPair()

    val nonce = new Array[Byte](32)
    Random.nextBytes(nonce)

    val sessionTokenOpt = sessionTokenStorage.get(remoteNodeId)

    val sharedSecret = sessionTokenOpt match {
      case Some(sessionToken) => sessionToken
      case None =>
        val agreement = new ECDHBasicAgreement
        agreement.init(nodeKey.getPrivate)
        agreement.calculateAgreement(remotePubKey.asInstanceOf[ECPublicKeyParameters]).toByteArray
    }

    val ephemeralPubKey = ECIESPublicKeyEncoder.getEncoded(ephemeralKey.getPublic.asInstanceOf[ECPublicKeyParameters])
    val ephemeralPublicHash = sha3(ephemeralPubKey, 1, 64)

    val publicKey = nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ

    val knownPeer = sessionTokenStorage.contains(remoteNodeId)

    val signature = {
      val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
      signer.init(true, ephemeralKey.getPrivate)
      val components = signer.generateSignature(xor(sharedSecret, nonce))
      ECDSASignature(components(0), components(1), 1.toByte) // TODO: 1.toByte
    }

    AuthInitiateMessage(signature, ByteString(ephemeralPublicHash), publicKey, ByteString(nonce), knownPeer)
  }

  def decodeAuthInitiateMessage(remoteNodeId: String, data: Array[Byte]) = {
    val plaintext = ECIESCoder.decrypt(nodeKey.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD, data)
    AuthInitiateMessage.decode(plaintext)
  }

  def generateKeyPair(): AsymmetricCipherKeyPair = {
    val generator = new ECKeyPairGenerator
    generator.init(new ECKeyGenerationParameters(curve, new SecureRandom))
    generator.generateKeyPair()
  }

}
