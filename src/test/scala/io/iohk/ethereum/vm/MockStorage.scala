package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.UInt256

object MockStorage {
  val Empty = MockStorage()

  def fromSeq(words: Seq[UInt256]): MockStorage = {
    val map = words.zipWithIndex.map { case (w, i) => BigInt(i) -> w.toBigInt }.toMap
    MockStorage(map)
  }
}

case class MockStorage(data: Map[BigInt, BigInt] = Map()) extends Storage[MockStorage] {
  def store(addr: BigInt, value: BigInt): MockStorage = {
    val updated =
      if (UInt256(value) == UInt256.Zero)
        data - addr
      else
        data + (addr -> value)

    copy(data = updated)
  }

  def load(addr: BigInt): BigInt =
    data.getOrElse(addr, UInt256.Zero)

  def isEmpty: Boolean =
    data.isEmpty
}
