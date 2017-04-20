package io.iohk.ethereum.network.rlpx

import java.math.BigInteger

import akka.util.ByteString
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.crypto.ECDSASignature.{RLength, SLength}
import org.spongycastle.util.BigIntegers.asUnsignedByteArray

trait AuthInitiateEcdsaCodec {

  def encodeECDSA(sig: ECDSASignature): ByteString = {
    import sig._

    val recId: Byte = (v - 27).toByte

    ByteString(
      asUnsignedByteArray(r).reverse.padTo(RLength, 0.toByte).reverse ++
        asUnsignedByteArray(s).reverse.padTo(SLength, 0.toByte).reverse ++
        Array(recId))
  }

  def decodeECDSA(input: Array[Byte]): ECDSASignature = {
    val SIndex = 32
    val VIndex = 64

    val r = input.take(RLength)
    val s = input.slice(SIndex, SIndex + SLength)
    val v = input(VIndex) + 27
    ECDSASignature(new BigInteger(1, r), new BigInteger(1, s), v.toByte)
  }
}
