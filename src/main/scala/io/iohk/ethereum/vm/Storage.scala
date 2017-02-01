package io.iohk.ethereum.vm

object Storage {

  def fromSeq(words: Seq[DataWord]): Storage = {
    val map = words.zipWithIndex.map { case (w, i) => DataWord(i) -> w }.toMap
    new Storage(map)
  }

  val Empty: Storage = new Storage()
}

/** Persistent storage of a transaction. Stores a hashmap of 256 bit keys
 *  mapped to 256 bit values.
 */
class Storage private(private val underlying: Map[DataWord, DataWord] = Map()) {

  def store(addr: DataWord, value: DataWord): Storage = new Storage(underlying + (addr -> value))

  def load(addr: DataWord): DataWord = underlying.getOrElse(addr, DataWord.Zero)

  def toMap: Map[DataWord, DataWord] = underlying

  override def equals(that: Any): Boolean =
    that match {
      case that: Storage => this.underlying == that.underlying
      case other => false
    }

  override def hashCode: Int = underlying.hashCode

  override def toString: String = underlying.toString.replace("Map", getClass.getSimpleName)
}
