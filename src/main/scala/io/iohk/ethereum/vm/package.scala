package io.iohk.ethereum

import io.iohk.ethereum.domain.UInt256

package object vm {

  /** Number of 32-byte UInt256s required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: BigInt): BigInt =
    if (n == 0) 0 else (n - 1) / UInt256.Size + 1
}
