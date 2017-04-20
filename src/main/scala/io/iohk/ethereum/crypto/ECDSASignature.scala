package io.iohk.ethereum.crypto

import java.math.BigInteger

import akka.util.ByteString
import org.spongycastle.asn1.x9.X9IntegerConverter
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.digests.SHA256Digest
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.spongycastle.math.ec.{ECCurve, ECPoint}

object ECDSASignature {

  val SLength = 32
  val RLength = 32
  val EncodedLength: Int = RLength + SLength + 1

  //only naming convention
  val negativePointSign: Byte = 27
  val newNegativePointSign: Byte = 35
  val positivePointSign: Byte = 28
  val newPositivePointSign: Byte = 36

  def apply(r: ByteString, s: ByteString, v: ByteString): ECDSASignature = {
    ECDSASignature(new BigInteger(1, r.toArray), new BigInteger(1, s.toArray), BigInt(v.toArray).toByte)
  }

  def sign(message: Array[Byte], keyPair: AsymmetricCipherKeyPair): ECDSASignature = {
    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
    signer.init(true, keyPair.getPrivate)
    val components = signer.generateSignature(message)
    val r = components(0)
    val s = ECDSASignature.canonicalise(components(1))
    val v = ECDSASignature
      .calculateV(r, s, keyPair, message)
      .getOrElse(throw new RuntimeException("Failed to calculate signature rec id"))

    ECDSASignature(r, s, v)
  }

  /**
    * new formula for calculating point sign post EIP 155 adoption
    * v = CHAIN_ID * 2 + 35 or v = CHAIN_ID * 2 + 36
    */
  private def getRecoveredPointSign(pointSign: Byte, chainId: Option[Byte]): Option[Int] = {
    chainId match {
      case Some(id) =>
        if (pointSign == negativePointSign || pointSign == (id * 2 + newNegativePointSign).toByte) {
          Some(negativePointSign)
        } else if (pointSign == positivePointSign || pointSign == (id * 2 + newPositivePointSign).toByte) {
          Some(positivePointSign)
        } else {
          None
        }
      case None => Some(pointSign)
    }
  }

  private def canonicalise(s: BigInteger): BigInteger = {
    val halfCurveOrder = curveParams.getN.shiftRight(1)
    if (s.compareTo(halfCurveOrder) > 0) curve.getN.subtract(s)
    else s
  }

  private def calculateV(r: BigInteger, s: BigInteger, key: AsymmetricCipherKeyPair, message: Array[Byte]): Option[Byte] = {
    val pubKey = key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)
    val recIdOpt = Seq(positivePointSign, negativePointSign).find { i =>
      recoverPubBytes(r, s, i, None, message).exists(java.util.Arrays.equals(_, pubKey))
    }
    recIdOpt
  }

  private def recoverPubBytes(r: BigInteger, s: BigInteger, recId: Byte, chainId: Option[Byte], message: Array[Byte]): Option[Array[Byte]] = {
    val order = curve.getCurve.getOrder
    val xCoordinate = r //ignore case when x = r + order because it is negligibly improbable
    val curveFp = curve.getCurve.asInstanceOf[ECCurve.Fp]
    val prime = curveFp.getQ

    getRecoveredPointSign(recId, chainId).flatMap { recovery =>
      if (xCoordinate.compareTo(prime) < 0) {
        val R = constructPoint(xCoordinate, recovery)
        if (R.multiply(order).isInfinity) {
          val e = new BigInteger(1, message)
          val rInv = r.modInverse(order)
          //Q = r^(-1)(sR - eG)
          val q = R.multiply(s).subtract(curve.getG.multiply(e)).multiply(rInv)
          Some(q.getEncoded(false))
        } else None
      } else None
    }
  }

  private def constructPoint(xCoordinate: BigInteger, recId: Int): ECPoint = {
    val x9 = new X9IntegerConverter
    val compEnc = x9.integerToBytes(xCoordinate, 1 + x9.getByteLength(curve.getCurve))
    compEnc(0) = if (recId == ECDSASignature.positivePointSign) 3.toByte else 2.toByte
    curve.getCurve.decodePoint(compEnc)
  }

}

case class ECDSASignature(r: BigInteger, s: BigInteger, v: Byte) {

  def publicKey(message: Array[Byte], chainId: Option[Byte] = None): Option[Array[Byte]] =
    ECDSASignature.recoverPubBytes(r, s, v, chainId, message)

  def publicKey(message: ByteString): Option[ByteString] =
    ECDSASignature.recoverPubBytes(r, s, v, None, message.toArray[Byte]).map(ByteString(_))
}
