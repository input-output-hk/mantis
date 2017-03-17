package io.iohk.ethereum.vm

object MockStorage {
  val Empty = MockStorage()

  def fromSeq(words: Seq[DataWord]): MockStorage = {
    val map = words.zipWithIndex.map { case (w, i) => DataWord(i) -> w }.toMap
    MockStorage(map)
  }
}

case class MockStorage(data: Map[DataWord, DataWord] = Map()) extends Storage[MockStorage] {
  def store(addr: DataWord, value: DataWord): MockStorage = {
    val updated =
      if (value == DataWord.Zero)
        data - addr
      else
        data + (addr -> value)

    copy(data = updated)
  }

  def load(addr: DataWord): DataWord =
    data.getOrElse(addr, DataWord.Zero)
}
