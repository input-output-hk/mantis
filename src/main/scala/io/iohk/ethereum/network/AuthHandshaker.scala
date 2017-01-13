package io.iohk.ethereum.network

import java.net.URI

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.utils.ByteUtils
import io.iohk.ethereum.utils.ByteUtils._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.agreement.ECDHBasicAgreement
import org.spongycastle.crypto.digests.{KeccakDigest, SHA256Digest}
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.crypto.signers.{HMacDSAKCalculator, ECDSASigner}
import org.spongycastle.math.ec.ECPoint

sealed trait AuthHandshakeResult
case object AuthHandshakeError extends AuthHandshakeResult
case class AuthHandshakeSuccess(secrets: Secrets) extends AuthHandshakeResult

class Secrets(
    val aes: Array[Byte],
    val mac: Array[Byte],
    val token: Array[Byte],
    val egressMac: KeccakDigest,
    val ingressMac: KeccakDigest)

object AuthHandshaker {

  val NonceSize = 32
  val MacSize = 256
  val SecretSize = 32

  def apply(nodeKey: AsymmetricCipherKeyPair): AuthHandshaker = {
    val nonce = ByteUtils.secureRandomBytes(NonceSize)
    AuthHandshaker(nodeKey, ByteString(nonce))
  }

}

case class AuthHandshaker(
    nodeKey: AsymmetricCipherKeyPair,
    nonce: ByteString,
    ephemeralKey: AsymmetricCipherKeyPair = generateKeyPair(),
    isInitiator: Boolean = false,
    initiateMessageOpt: Option[AuthInitiateMessage] = None,
    initiatePacketOpt: Option[ByteString] = None,
    responseMessageOpt: Option[AuthResponseMessage] = None,
    responsePacketOpt: Option[ByteString] = None) {

  import AuthHandshaker._

  def initiate(uri: URI): (ByteString, AuthHandshaker) = {
    val remotePubKey = publicKeyFromNodeId(uri.getUserInfo)
    val message = createAuthInitiateMessage(remotePubKey)
    val encryptedPacket = ByteString(ECIESCoder.encrypt(remotePubKey, message.encoded.toArray, None))

    (encryptedPacket, copy(isInitiator = true, initiateMessageOpt = Some(message), initiatePacketOpt = Some(encryptedPacket)))
  }

  def handleResponseMessage(data: ByteString): AuthHandshakeResult = {
    val plaintext = ECIESCoder.decrypt(nodeKey.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD, data.toArray)
    val message = AuthResponseMessage.decode(plaintext)

    copy(
      responseMessageOpt = Some(message),
      responsePacketOpt = Some(data)).finalizeHandshake()
  }

  def handleInitialMessage(data: ByteString): (ByteString, AuthHandshakeResult) = {
    val plaintext = ECIESCoder.decrypt(nodeKey.getPrivate.asInstanceOf[ECPrivateKeyParameters].getD, data.toArray)
    val message = AuthInitiateMessage.decode(plaintext)

    val response = createAuthResponseMessage()
    val encryptedPacket = ByteString(ECIESCoder.encrypt(message.publicKey, response.encoded.toArray, None))

    val handshakeResult = copy(
      initiateMessageOpt = Some(message),
      initiatePacketOpt = Some(data),
      responseMessageOpt = Some(response),
      responsePacketOpt = Some(encryptedPacket)).finalizeHandshake()

    (encryptedPacket, handshakeResult)
  }

  private def createAuthInitiateMessage(remotePubKey: ECPoint) = {
    // TODO: handle the case when the peer is known
    val sessionTokenOpt: Option[Array[Byte]] = None
    val knownPeer = sessionTokenOpt.isDefined

    val sharedSecret = sessionTokenOpt match {
      case Some(sessionToken) => sessionToken
      case None =>
        val agreement = new ECDHBasicAgreement
        agreement.init(nodeKey.getPrivate)
        bigIntegerToBytes(agreement.calculateAgreement(new ECPublicKeyParameters(remotePubKey, curve)), NonceSize)
    }

    val ephemeralPubKey = ephemeralKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)
    val ephemeralPublicHash = sha3(ephemeralPubKey, 1, 64)

    val publicKey = nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ

    val signature = {
      val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
      signer.init(true, ephemeralKey.getPrivate)
      val messageToSign = xor(sharedSecret, nonce.toArray)
      val components = signer.generateSignature(messageToSign)
      val r = components(0)
      val s = ECDSASignature.canonicalise(components(1))
      val v = ECDSASignature
        .calculateV(r, s, ephemeralKey, messageToSign)
        .getOrElse(throw new RuntimeException("Failed to calculate signature rec id"))

      ECDSASignature(r, s, v)
    }

    AuthInitiateMessage(signature, ByteString(ephemeralPublicHash), publicKey, nonce, knownPeer)
  }

  private def createAuthResponseMessage() = {
    // TODO: handle the case when the peer is known
    AuthResponseMessage(ephemeralKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ, nonce, knownPeer = false)
  }

  private def finalizeHandshake(): AuthHandshakeResult = {
    // TODO: handle the case when the peer is known
    val successOpt = for {
      initiatePacket <- initiatePacketOpt
      initiateMessage <- initiateMessageOpt
      responsePacket <- responsePacketOpt
      responseMessage <- responseMessageOpt
    } yield {

      val remoteEphemeralKey =
        if (isInitiator) responseMessage.ephemeralPublicKey
        else {
          val agreement = new ECDHBasicAgreement
          agreement.init(nodeKey.getPrivate)
          val sharedSecret = agreement.calculateAgreement(new ECPublicKeyParameters(initiateMessage.publicKey, curve))

          val token = bigIntegerToBytes(sharedSecret, NonceSize)
          val signed = xor(token, initiateMessage.nonce.toArray)

          val signaturePubBytes =
            ECDSASignature.recoverPubBytes(
              r = initiateMessage.signature.r,
              s = initiateMessage.signature.s,
              recId = ECDSASignature.recIdFromSignatureV(initiateMessage.signature.v),
              message = signed).get

          curve.getCurve.decodePoint(signaturePubBytes)
        }

      val secretScalar = {
        val agreement = new ECDHBasicAgreement
        agreement.init(ephemeralKey.getPrivate)
        agreement.calculateAgreement(new ECPublicKeyParameters(remoteEphemeralKey, curve))
      }

      val agreedSecret = bigIntegerToBytes(secretScalar, SecretSize)
      val sharedSecret = sha3(agreedSecret, sha3(responseMessage.nonce.toArray, initiateMessage.nonce.toArray))

      val aesSecret = sha3(agreedSecret, sharedSecret)

      val macSecret = sha3(agreedSecret, aesSecret)

      val mac1 = new KeccakDigest(MacSize)
      mac1.update(xor(macSecret, responseMessage.nonce.toArray), 0, macSecret.length)
      val buf = new Array[Byte](32)
      new KeccakDigest(mac1).doFinal(buf, 0)
      mac1.update(initiatePacket.toArray, 0, initiatePacket.toArray.length)
      new KeccakDigest(mac1).doFinal(buf, 0)

      val mac2 = new KeccakDigest(MacSize)
      mac2.update(xor(macSecret, initiateMessage.nonce.toArray), 0, macSecret.length)
      new KeccakDigest(mac2).doFinal(buf, 0)
      mac2.update(responsePacket.toArray, 0, responsePacket.toArray.length)
      new KeccakDigest(mac2).doFinal(buf, 0)

      val (egressMacSecret, ingressMacSecret) =
        if (isInitiator) (mac1, mac2)
        else (mac2, mac1)

      AuthHandshakeSuccess(new Secrets(
        aes = aesSecret,
        mac = sha3(agreedSecret, aesSecret),
        token = sha3(sharedSecret),
        egressMac = egressMacSecret,
        ingressMac = ingressMacSecret))
    }

    successOpt getOrElse AuthHandshakeError
  }

}
