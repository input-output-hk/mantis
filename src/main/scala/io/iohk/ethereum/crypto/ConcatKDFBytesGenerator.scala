package io.iohk.ethereum.crypto

import org.spongycastle.crypto.params.{ISO18033KDFParameters, KDFParameters}
import org.spongycastle.crypto.{DataLengthException, Digest}
import org.spongycastle.util.Pack

/**
  * Basic KDF generator for derived keys and ivs as defined by NIST SP 800-56A.
  */

/**
  * Construct a KDF Parameters generator.
  * <p>
  *
  * @param counterStart
  * value of counter.
  * @param digest
  * the digest to be used as the source of derived keys.
  */

class ConcatKDFBytesGenerator(counterStart: Int = 1: Int, digest: Digest, param: KDFParameters) {

  val shared: Array[Byte] = param.getSharedSecret
  val iv: Array[Byte] = param.getIV

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

    val cThreshold = (oBytes + outLen - 1) / outLen
    val dig = new Array[Byte](digest.getDigestSize)
    val C = new Array[Byte](4)
    Pack.intToBigEndian(counterStart, C, 0)
    var counterBase = counterStart & ~0xFF


    (0 until cThreshold).foreach { _ =>
      digest.update(C, 0, C.length)
      digest.update(shared, 0, shared.length)
      digest.update(iv, 0, iv.length)
      digest.doFinal(dig, 0)
      if (currnetLen > outLen) {
        System.arraycopy(dig, 0, out, currnetOutOff, outLen)
        currnetOutOff += outLen
        currnetLen -= outLen
      } else {
        System.arraycopy(dig, 0, out, currnetOutOff, currnetLen)
      }

      C(3) = (C(3) + 1.toByte).toByte

      if (C(3) == 0) {
        counterBase += 0x100
        Pack.intToBigEndian(counterBase, C, 0)
      }
    }


    digest.reset()
    oBytes.toInt
  }
}