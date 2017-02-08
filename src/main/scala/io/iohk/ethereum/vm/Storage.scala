package io.iohk.ethereum.vm

object Storage {

  def fromSeq(words: Seq[DataWord]): Storage = {
    val map = words.zipWithIndex.map { case (w, i) => DataWord(i) -> w }.toMap
    new Storage(map)
  }

  val Empty: Storage = new Storage()
}

/** Persistent storage of a transaction. May be viewed as a map of 256 bit keys
  * to 256 bit values.
  * This is just a representation of storage changes occuring during program execution. The
  * actual persistence is not this class's resposibility.
  */
class Storage private(private val underlying: Map[DataWord, DataWord] = Map()) {

  /** Stores new value in an underlying hashmap.
    * If DataWord.Zero is passed as a value then a corresponding key is removed from the hashmap
    */
  def store(addr: DataWord, value: DataWord): Storage = {
    val newUnderlying: Map[DataWord, DataWord] = value match {
      case DataWord.Zero => underlying - addr
      case dw => underlying + (addr -> value)
    }
    new Storage(newUnderlying)
  }

  /** Retrieves a value from an underlying hashmap for a given key.
    * Returns a DataWord.Zero if a value for given key does not exist
    */
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
