package io.iohk.ethereum.vm

object Storage {

  def fromSeq(words: Seq[DataWord]): Storage = {
    val map = words.zipWithIndex.map { case (w, i) => DataWord(i) -> w }.toMap
    new Storage(map)
  }

  val Empty: Storage = new Storage()
}

class Storage private(underlying: Map[DataWord, DataWord] = Map()) {

  def store(addr: DataWord, value: DataWord): Storage = new Storage(underlying + (addr -> value))

  def load(addr: DataWord): DataWord = underlying.getOrElse(addr, DataWord.Zero)

  def toMap: Map[DataWord, DataWord] = underlying

}
