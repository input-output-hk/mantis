package io.iohk.ethereum.crypto

import java.math.BigInteger

import org.spongycastle.util.BigIntegers._

object ECDSASignature {

  def decode(input: Array[Byte]) = {
    val r = input.take(32)
    val s = input.slice(32, 32 + 32)
    val v = input(64) + 27
    ECDSASignature(new BigInteger(1, r), new BigInteger(1, s), v.toByte)
  }

}

case class ECDSASignature(r: BigInteger, s: BigInteger, v: Byte) {

  def encode() = {
    asUnsignedByteArray(r).reverse.padTo(32, 0.toByte).reverse ++
    asUnsignedByteArray(s).reverse.padTo(32, 0.toByte).reverse ++
    Array(recId)
  }

  private def recId = {
    if (v >= 31) (v - 31).toByte
    else (v - 27).toByte
  }

  def canonicalise() = {
    val halfCurveOrder = curveParams.getN.shiftRight(1)

    if (s.compareTo(halfCurveOrder) > 0) copy(s = curve.getN.subtract(s))
    else this
  }

}
