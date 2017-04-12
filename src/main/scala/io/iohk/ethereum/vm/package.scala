package io.iohk.ethereum

package object vm {

  /**
    * Number of 32-byte UInt256s required to hold n bytes (~= math.ceil(n / 32))
    */
  def wordsForBytes(n: UInt256): UInt256 =
    if (n.isZero) 0 else (n - 1) / UInt256.Size + 1
}
