package io.iohk.ethereum.crypto

import org.spongycastle.crypto.digests.KeccakDigest

object Keccak {

  def hash256(in: Array[Byte]) : Array[Byte] = {
    hash(256, in)
  }

  private def hash(length: Int, in: Array[Byte]): Array[Byte] = {
    val sha3 = new KeccakDigest(length)
    sha3.update(in, 0, in.length)
    val output = new Array[Byte](length / 8)
    sha3.doFinal(output, 0)
    output
  }
}
