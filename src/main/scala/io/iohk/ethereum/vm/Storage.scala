package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.crypto.kec256

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

  /** Stores new value in an underlying hashmap */
  def store(addr: DataWord, value: DataWord): Storage =
    new Storage(underlying + (addr -> value))

  /** Retrieves a value from an underlying hashmap for a given key.
    * Returns a DataWord.Zero if a value for given key does not exist
    */
  def load(addr: DataWord): DataWord = underlying.getOrElse(addr, DataWord.Zero)

  /** Calculates new root hash based on storage contents */
  lazy val storageRoot: ByteString =
    ByteString(kec256(toString.getBytes)) // TODO: I'm a mock implementation, change me

  def toMap: Map[DataWord, DataWord] = underlying

  def size: Int = underlying.size

  override def equals(that: Any): Boolean =
    that match {
      case that: Storage => this.underlying == that.underlying
      case other => false
    }

  override def hashCode: Int = underlying.hashCode

  override def toString: String = underlying.toString.replace("Map", getClass.getSimpleName)
}
