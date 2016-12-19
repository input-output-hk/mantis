package io.iohk.ethereum.network

import fr.cryptohash.Keccak256
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.agreement.ECDHBasicAgreement
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.params.{ECPrivateKeyParameters, ECPublicKeyParameters}
import org.spongycastle.crypto.signers.{HMacDSAKCalculator, ECDSASigner}
import org.spongycastle.math.ec.ECPoint
import org.spongycastle.util.BigIntegers
import BigIntegers.asUnsignedByteArray

case class AuthInitiateMessage(
    nodeKey: AsymmetricCipherKeyPair,
    ephemeralKey: AsymmetricCipherKeyPair,
    remotePubKey: ECPoint,
    nonce: Array[Byte],
    sessionTokenOpt: Option[Array[Byte]]) {

  def sharedSecret = sessionTokenOpt match {
    case Some(sessionToken) => sessionToken
    case None =>
      val agreement = new ECDHBasicAgreement
      agreement.init(nodeKey.getPrivate)
      agreement.calculateAgreement(new ECPublicKeyParameters(remotePubKey, curve)).toByteArray
  }

  def signature: Array[Byte] = {
    val message = xor(sharedSecret, nonce)

    val ephemeralPrivKey = ephemeralKey.getPrivate.asInstanceOf[ECPrivateKeyParameters]

    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest()))
    val privKeyParams = new ECPrivateKeyParameters(ephemeralPrivKey.getD, curve)
    signer.init(true, privKeyParams)
    val components = signer.generateSignature(message)
    val rComponent = components(0)
    val sComponent = components(1)
    val halfCurveOrder = curveParams.getN.shiftRight(1)
    val sComponentCanonicalised =
      if (sComponent.compareTo(halfCurveOrder) > 0) curve.getN.subtract(sComponent)
      else sComponent

    val recId: Byte = 1 // TODO (port whole ECKey)

    asUnsignedByteArray(rComponent).padTo(32, 0.toByte) ++
      asUnsignedByteArray(sComponentCanonicalised).padTo(32, 0.toByte) ++
      Array[Byte](recId)
  }

  def encode(): Array[Byte] = {
    val ephemeralPubKey = ephemeralKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)
    val ephemeralKeyMac = sha3(ephemeralPubKey, 1, 64)

    val publicKeyBytes = nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)

    val knownPeerByte: Byte = if (sessionTokenOpt.isDefined) 0x01 else 0x00

    signature ++ ephemeralKeyMac ++ publicKeyBytes ++ nonce ++ Array(knownPeerByte)
  }

  private def xor(a: Array[Byte], b: Array[Byte]): Array[Byte] = {
    (a zip b) map { case (b1, b2) => (b1 ^ b2).toByte }
  }

  private def sha3(input: Array[Byte], start: Int, length: Int): Array[Byte] = {
    val digest = new Keccak256
    digest.update(input, start, length)
    digest.digest
  }

}
