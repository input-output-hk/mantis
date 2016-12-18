package io.iohk.ethereum.crypto

import org.spongycastle.crypto.params.MGFParameters
import org.spongycastle.crypto.{DataLengthException, DerivationFunction, DerivationParameters, Digest}

/**
  * This class is borrowed from spongycastle project
  * The only change made is addition of 'counterStart' parameter to
  * conform to Crypto++ capabilities
  */
class MGF1BytesGeneratorExt(var digest: Digest, var counterStart: Int) extends DerivationFunction {
  this.hLen = digest.getDigestSize
  private var seed:Array[Byte] = null
  private var hLen = 0

  def init(param: DerivationParameters) {
    if (!param.isInstanceOf[MGFParameters]) throw new IllegalArgumentException("MGF parameters required for MGF1Generator")
    else {
      val p = param.asInstanceOf[MGFParameters]
      this.seed = p.getSeed
    }
  }

  def getDigest: Digest = this.digest

  private def ItoOSP(i: Int, sp: Array[Byte]) {
    sp(0) = (i >>> 24).toByte
    sp(1) = (i >>> 16).toByte
    sp(2) = (i >>> 8).toByte
    sp(3) = (i >>> 0).toByte
  }

  @throws[DataLengthException]
  @throws[IllegalArgumentException]
  def generateBytes(out: Array[Byte], outOff: Int, len: Int): Int = if (out.length - len < outOff) throw new DataLengthException("output buffer too small")
  else {
    val hashBuf = new Array[Byte](this.hLen)
    val C = new Array[Byte](4)
    var counter = 0
    var hashCounter = counterStart
    this.digest.reset()
    if (len > this.hLen) do {
      this.ItoOSP({
        hashCounter += 1; hashCounter - 1
      }, C)
      this.digest.update(this.seed, 0, this.seed.length)
      this.digest.update(C, 0, C.length)
      this.digest.doFinal(hashBuf, 0)
      System.arraycopy(hashBuf, 0, out, outOff + counter * this.hLen, this.hLen)
      counter += 1
    } while (counter < len / this.hLen)
    if (counter * this.hLen < len) {
      this.ItoOSP(hashCounter, C)
      this.digest.update(this.seed, 0, this.seed.length)
      this.digest.update(C, 0, C.length)
      this.digest.doFinal(hashBuf, 0)
      System.arraycopy(hashBuf, 0, out, outOff + counter * this.hLen, len - counter * this.hLen)
    }
    len
  }
}
