package io.iohk.ethereum.vm

object Storage {

  def fromSeq(words: Seq[DataWord]): Storage = {
    val map = words.zipWithIndex.map { case (w, i) => DataWord(i) -> w }.toMap
    new Storage(map)
  }

  val Empty: Storage = new Storage()
}

class Storage private(private val underlying: Map[DataWord, DataWord] = Map()) {

  def store(addr: DataWord, value: DataWord): Storage = new Storage(underlying + (addr -> value))

  def load(addr: DataWord): DataWord = underlying.getOrElse(addr, DataWord.Zero)

  def toMap: Map[DataWord, DataWord] = underlying

  def size: Int = underlying.keys.map(_.intValue).foldLeft(-1)((max, x) => if (x > max) x else max) + 1

  override def equals(that: Any): Boolean =
    that match {
      case that: Storage => this.underlying == that.underlying
      case other => false
    }

  override def hashCode: Int = underlying.hashCode

  override def toString: String = underlying.toString.replace("Map", getClass.getSimpleName)
}
