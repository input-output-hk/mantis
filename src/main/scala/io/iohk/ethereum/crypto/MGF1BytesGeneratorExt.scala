package io.iohk.ethereum.crypto

import org.spongycastle.crypto.{DataLengthException, Digest}

/**
  * This class is borrowed from spongycastle project
  * The only change made is addition of 'counterStart' parameter to
  * conform to Crypto++ capabilities
  */
class MGF1BytesGeneratorExt(digest: Digest, counterStart: Int) {
  val hLen: Int = digest.getDigestSize

  private def ItoOSP(i: Int, sp: Array[Byte]) {
    sp(0) = (i >>> 24).toByte
    sp(1) = (i >>> 16).toByte
    sp(2) = (i >>> 8).toByte
    sp(3) = (i >>> 0).toByte
  }

  @throws[DataLengthException]
  @throws[IllegalArgumentException]
  def generateBytes(out: Array[Byte], outOff: Int, len: Int, seed: Array[Byte]): Int =
    if (out.length - len < outOff)
      throw new DataLengthException("output buffer too small")
    else {
      val hashBuf = new Array[Byte](hLen)
      val C = new Array[Byte](4)
      var counter = 0
      var hashCounter = counterStart
      digest.reset()
      if (len > hLen)
        do {
          ItoOSP(hashCounter, C)
          hashCounter += 1
          digest.update(seed, 0, seed.length)
          digest.update(C, 0, C.length)
          digest.doFinal(hashBuf, 0)
          System.arraycopy(hashBuf, 0, out, outOff + counter * hLen, hLen)
          counter += 1
        } while (counter < len / hLen)

      if (counter * hLen < len) {
        ItoOSP(hashCounter, C)
        digest.update(seed, 0, seed.length)
        digest.update(C, 0, C.length)
        digest.doFinal(hashBuf, 0)
        System.arraycopy(hashBuf, 0, out, outOff + counter * hLen, len - counter * hLen)
      }
      len
    }
}
