package io.iohk.ethereum.network.rlpx

import akka.util.ByteString

import org.bouncycastle.util.BigIntegers.asUnsignedByteArray

import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.crypto.ECDSASignature.RLength
import io.iohk.ethereum.crypto.ECDSASignature.SLength

trait AuthInitiateEcdsaCodec {

  def encodeECDSA(sig: ECDSASignature): ByteString = {
    import sig._

    val recoveryId: Byte = (v - 27).toByte

    ByteString(
      asUnsignedByteArray(r.bigInteger).reverse.padTo(RLength, 0.toByte).reverse ++
        asUnsignedByteArray(s.bigInteger).reverse.padTo(SLength, 0.toByte).reverse ++
        Array(recoveryId)
    )
  }

  def decodeECDSA(input: Array[Byte]): ECDSASignature = {
    val SIndex = 32
    val VIndex = 64

    val r = input.take(RLength)
    val s = input.slice(SIndex, SIndex + SLength)
    val v = input(VIndex) + 27
    ECDSASignature(BigInt(1, r), BigInt(1, s), v.toByte)
  }
}
