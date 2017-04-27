package io.iohk.ethereum.crypto

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
  //byte value that indicates that bytes representing ECC point are in uncompressed format, and should be decoded properly
  val uncompressedIndicator:Byte = 0x04

  //only naming convention
  val negativePointSign: Byte = 27
  val newNegativePointSign: Byte = 35
  val positivePointSign: Byte = 28
  val newPositivePointSign: Byte = 36

  val allowedPointSigns = Set(negativePointSign, positivePointSign)

  def apply(r: ByteString, s: ByteString, v: ByteString): ECDSASignature = {
    ECDSASignature(BigInt(1, r.toArray), BigInt(1, s.toArray), BigInt(v.toArray).toByte)
  }

  def sign(message: Array[Byte], keyPair: AsymmetricCipherKeyPair, chainId: Option[Byte] = None): ECDSASignature = {
    val signer = new ECDSASigner(new HMacDSAKCalculator(new SHA256Digest))
    signer.init(true, keyPair.getPrivate)
    val components = signer.generateSignature(message)
    val r = components(0)
    val s = ECDSASignature.canonicalise(components(1))
    val v = ECDSASignature
      .calculateV(r, s, keyPair, message)
      .getOrElse(throw new RuntimeException("Failed to calculate signature rec id"))

    val pointSign = chainId match {
      case Some(id) if v == negativePointSign => (id * 2 + newNegativePointSign).toByte
      case Some(id) if v == positivePointSign => (id * 2 + newPositivePointSign).toByte
      case None => v
    }

    ECDSASignature(r, s, pointSign)
  }

  /**
    * new formula for calculating point sign post EIP 155 adoption
    * v = CHAIN_ID * 2 + 35 or v = CHAIN_ID * 2 + 36
    */
  private def getRecoveredPointSign(pointSign: Byte, chainId: Option[Byte]): Option[Byte] = {
    (chainId match {
      case Some(id) =>
        if (pointSign == negativePointSign || pointSign == (id * 2 + newNegativePointSign).toByte) {
          Some(negativePointSign)
        } else if (pointSign == positivePointSign || pointSign == (id * 2 + newPositivePointSign).toByte) {
          Some(positivePointSign)
        } else {
          None
        }
      case None => Some(pointSign)
    }).filter(pointSign => allowedPointSigns.contains(pointSign))
  }

  private def canonicalise(s: BigInt): BigInt = {
    val halfCurveOrder: BigInt = curveParams.getN.shiftRight(1)
    if (s > halfCurveOrder) BigInt(curve.getN) - s
    else s
  }

  private def calculateV(r: BigInt, s: BigInt, key: AsymmetricCipherKeyPair, message: Array[Byte]): Option[Byte] = {
    //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
    val pubKey = key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false).tail
    val recIdOpt = Seq(positivePointSign, negativePointSign).find { i =>
      recoverPubBytes(r, s, i, None, message).exists(java.util.Arrays.equals(_, pubKey))
    }
    recIdOpt
  }

  private def recoverPubBytes(r: BigInt, s: BigInt, recId: Byte, chainId: Option[Byte], message: Array[Byte]): Option[Array[Byte]] = {
    val order = curve.getCurve.getOrder
    //ignore case when x = r + order because it is negligibly improbable
    //says: https://github.com/paritytech/rust-secp256k1/blob/f998f9a8c18227af200f0f7fdadf8a6560d391ff/depend/secp256k1/src/ecdsa_impl.h#L282
    val xCoordinate = r
    val curveFp = curve.getCurve.asInstanceOf[ECCurve.Fp]
    val prime = curveFp.getQ

    getRecoveredPointSign(recId, chainId).flatMap { recovery =>
      if (xCoordinate.compareTo(prime) < 0) {
        val R = constructPoint(xCoordinate, recovery)
        if (R.multiply(order).isInfinity) {
          val e = BigInt(1, message)
          val rInv = r.modInverse(order)
          //Q = r^(-1)(sR - eG)
          val q = R.multiply(s.bigInteger).subtract(curve.getG.multiply(e.bigInteger)).multiply(rInv.bigInteger)
          //byte 0 of encoded ECC point indicates that it is uncompressed point, it is part of spongycastle encoding
          Some(q.getEncoded(false).tail)
        } else None
      } else None
    }
  }

  private def constructPoint(xCoordinate: BigInt, recId: Int): ECPoint = {
    val x9 = new X9IntegerConverter
    val compEnc = x9.integerToBytes(xCoordinate.bigInteger, 1 + x9.getByteLength(curve.getCurve))
    compEnc(0) = if (recId == ECDSASignature.positivePointSign) 3.toByte else 2.toByte
    curve.getCurve.decodePoint(compEnc)
  }

}

/**
  * ECDSASignature r and s are same as in documentation where signature is represented by tuple (r, s)
  * @param r - x coordinate of ephemeral public key modulo curve order N
  * @param s - part of the signature calculated with signer private key
  * @param v - public key recovery id
  */
case class ECDSASignature(r: BigInt, s: BigInt, v: Byte) {

  /**
    * returns ECC point encoded with on compression and without leading byte indicating compression
    * @param message message to be signed
    * @param chainId optional value if you want new signing schema with recovery id calculated with chain id
    * @return
    */
  def publicKey(message: Array[Byte], chainId: Option[Byte] = None): Option[Array[Byte]] =
    ECDSASignature.recoverPubBytes(r, s, v, chainId, message)

  /**
    * returns ECC point encoded with on compression and without leading byte indicating compression
    * @param message message to be signed
    * @return
    */
  def publicKey(message: ByteString): Option[ByteString] =
    ECDSASignature.recoverPubBytes(r, s, v, None, message.toArray[Byte]).map(ByteString(_))
}
