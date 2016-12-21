package io.iohk.ethereum.crypto

import java.math.BigInteger

import akka.util.ByteString
import org.spongycastle.asn1.x9.X9IntegerConverter
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.math.ec.{ECAlgorithms, ECPoint, ECCurve}
import org.spongycastle.util.BigIntegers._

object ECDSASignature {

  def decode(input: Array[Byte]) = {
    val r = input.take(32)
    val s = input.slice(32, 32 + 32)
    val v = input(64) + 27
    ECDSASignature(new BigInteger(1, r), new BigInteger(1, s), v.toByte)
  }

  def calculateV(r: BigInteger, s: BigInteger, key: AsymmetricCipherKeyPair, message: Array[Byte]): Option[Byte] = {

    def decompressKey(xBN: BigInteger, yBit: Boolean): ECPoint = {
      val x9 = new X9IntegerConverter
      val compEnc = x9.integerToBytes(xBN, 1 + x9.getByteLength(curve.getCurve))
      compEnc(0) = if (yBit) 3.toByte else 2.toByte
      curve.getCurve.decodePoint(compEnc)
    }

    def recoverPubBytes(recId: Int): Option[Array[Byte]] = {
      val n = curve.getN
      val i = BigInteger.valueOf(recId.toLong / 2)
      val x = r.add(i.multiply(n))
      val curveFp = curve.getCurve.asInstanceOf[ECCurve.Fp]
      val prime = curveFp.getQ
      if (x.compareTo(prime) < 0) {
        val R = decompressKey(x, (recId & 1) == 1)
        if (R.multiply(n).isInfinity) {
          val e = new BigInteger(1, message)
          val eInv = BigInteger.ZERO.subtract(e).mod(n)
          val rInv = r.modInverse(n)
          val srInv = rInv.multiply(s).mod(n)
          val eInvrInv = rInv.multiply(eInv).mod(n)
          val q = ECAlgorithms.sumOfTwoMultiplies(curve.getG, eInvrInv, R, srInv).asInstanceOf[ECPoint.Fp]
          Some(q.getEncoded(false))
        } else None
      } else None
    }

    val pubKey = key.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)

    val recIdOpt = (0 until 4).find { i =>
      recoverPubBytes(i).exists(java.util.Arrays.equals(_, pubKey))
    }

    recIdOpt.map(recId => (recId + 27).toByte)
  }

}

case class ECDSASignature(r: BigInteger, s: BigInteger, v: Byte) {

  def encode() = {
    ByteString(
      asUnsignedByteArray(r).reverse.padTo(32, 0.toByte).reverse ++
      asUnsignedByteArray(s).reverse.padTo(32, 0.toByte).reverse ++
      Array(
        if (v >= 31) (v - 31).toByte
        else (v - 27).toByte))
  }

  def canonicalise() = {
    val halfCurveOrder = curveParams.getN.shiftRight(1)

    if (s.compareTo(halfCurveOrder) > 0) copy(s = curve.getN.subtract(s))
    else this
  }

}
