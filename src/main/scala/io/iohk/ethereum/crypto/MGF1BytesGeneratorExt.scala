package io.iohk.ethereum.crypto

import akka.util.ByteString
import org.spongycastle.crypto.{DataLengthException, Digest}

/**
  * This class is borrowed from spongycastle project
  * The only change made is addition of 'counterStart' parameter to
  * conform to Crypto++ capabilities
  */
class MGF1BytesGeneratorExt(digest: Digest, counterStart: Int) {
  val digestSize: Int = digest.getDigestSize

  private def ItoOSP(i: Int, sp: Array[Byte]) {
    sp(0) = (i >>> 24).toByte
    sp(1) = (i >>> 16).toByte
    sp(2) = (i >>> 8).toByte
    sp(3) = (i >>> 0).toByte
  }

  @throws[DataLengthException]
  @throws[IllegalArgumentException]
  def generateBytes(outputLength: Int, seed: Array[Byte]): ByteString = {
    val hashBuf = new Array[Byte](digestSize)
    val counterValue = new Array[Byte](4)

    digest.reset()

    (0 until (outputLength / digestSize + 1)).map { i =>
      ItoOSP(counterStart + i, counterValue)
      digest.update(seed, 0, seed.length)
      digest.update(counterValue, 0, counterValue.length)
      digest.doFinal(hashBuf, 0)

      val spaceLeft = outputLength - (i * digestSize)

      if (spaceLeft > digestSize) {
        ByteString(hashBuf)
      } else {
        ByteString(hashBuf).dropRight(digestSize - spaceLeft)
      }
    }.reduce[ByteString] { case (a, b) => a ++ b }
  }
}
