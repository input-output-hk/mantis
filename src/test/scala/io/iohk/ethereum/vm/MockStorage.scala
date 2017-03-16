package io.iohk.ethereum.vm

object MockStorage {
  val Empty = MockStorage()

  def fromSeq(words: Seq[UInt256]): MockStorage = {
    val map = words.zipWithIndex.map { case (w, i) => UInt256(i) -> w }.toMap
    MockStorage(map)
  }
}

case class MockStorage(data: Map[UInt256, UInt256] = Map()) extends Storage[MockStorage] {
  def store(addr: UInt256, value: UInt256): MockStorage = {
    val updated =
      if (value == UInt256.Zero)
        data - addr
      else
        data + (addr -> value)

    copy(data = updated)
  }

  def load(addr: UInt256): UInt256 =
    data.getOrElse(addr, UInt256.Zero)
}
