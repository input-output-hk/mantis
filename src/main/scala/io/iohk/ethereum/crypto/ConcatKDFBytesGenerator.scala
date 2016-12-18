package io.iohk.ethereum.crypto

import org.spongycastle.crypto.params.{ISO18033KDFParameters, KDFParameters}
import org.spongycastle.crypto.{DataLengthException, DerivationParameters, Digest, DigestDerivationFunction}
import org.spongycastle.util.Pack

/**
  * Basic KDF generator for derived keys and ivs as defined by NIST SP 800-56A.
  */
class ConcatKDFBytesGenerator protected(var counterStart: Int, var digest: Digest) extends DigestDerivationFunction {

  /**
    * Construct a KDF Parameters generator.
    * <p>
    *
    * @param counterStart
    * value of counter.
    * @param digest
    * the digest to be used as the source of derived keys.
    */

  private var shared: Array[Byte] = null
  private var iv: Array[Byte] = null

  def this(digest: Digest) {
    this(1, digest)
  }

  def init(param: DerivationParameters) {
    if (param.isInstanceOf[KDFParameters]) {
      val p = param.asInstanceOf[KDFParameters]
      shared = p.getSharedSecret
      iv = p.getIV
    }
    else if (param.isInstanceOf[ISO18033KDFParameters]) {
      val p = param.asInstanceOf[ISO18033KDFParameters]
      shared = p.getSeed
      iv = null
    }
    else throw new IllegalArgumentException("KDF parameters required for KDF2Generator")
  }

  /**
    * return the underlying digest.
    */
  def getDigest: Digest = digest

  /**
    * fill len bytes of the output buffer with bytes generated from the
    * derivation function.
    *
    * @throws IllegalArgumentException
    * if the size of the request will cause an overflow.
    * @throws DataLengthException
    * if the out buffer is too small.
    */
  @throws[DataLengthException]
  @throws[IllegalArgumentException]
  def generateBytes(out: Array[Byte], outOff: Int, len: Int): Int = {
    if ((out.length - len) < outOff) throw new DataLengthException("output buffer too small")

    var currnetOutOff = outOff
    var currnetLen = len

    val oBytes = currnetLen
    val outLen = digest.getDigestSize
    //
    // this is at odds with the standard implementation, the
    // maximum value should be hBits * (2^32 - 1) where hBits
    // is the digest output size in bits. We can't have an
    // array with a long index at the moment...
    //
    if (oBytes > ((2L << 32) - 1)) throw new IllegalArgumentException("Output length too large")
    val cThreshold = ((oBytes + outLen - 1) / outLen).toInt
    val dig = new Array[Byte](digest.getDigestSize)
    val C = new Array[Byte](4)
    Pack.intToBigEndian(counterStart, C, 0)
    var counterBase = counterStart & ~0xFF
    var i = 0
    while (i < cThreshold) {
      {
        digest.update(C, 0, C.length)
        digest.update(shared, 0, shared.length)
        if (iv != null) digest.update(iv, 0, iv.length)
        digest.doFinal(dig, 0)
        if (currnetLen > outLen) {
          System.arraycopy(dig, 0, out, currnetOutOff, outLen)
          currnetOutOff += outLen
          currnetLen -= outLen
        }
        else System.arraycopy(dig, 0, out, currnetOutOff, currnetLen)

        if ( {
          C(3) = (C(3) + 1.toByte).toByte //TODO test if it overflows correctly in java here was    if (++C[3] == 0)
          C(3)
        } == 0) {
          counterBase += 0x100
          Pack.intToBigEndian(counterBase, C, 0)
        }
      }

      i += 1
    }
    digest.reset()
    oBytes.toInt
  }
}