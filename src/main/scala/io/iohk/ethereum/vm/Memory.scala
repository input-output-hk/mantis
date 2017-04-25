package io.iohk.ethereum.vm

import akka.util.ByteString

object Memory {

  def empty: Memory = new Memory(ByteString())

  private def zeros(size: Int): ByteString = ByteString(Array.fill[Byte](size)(0))
}

/**
 * Volatile memory with 256 bit address space.
 * Every mutating operation on a Memory returns a new updated copy of it.
 *
 * Related reading:
 * https://solidity.readthedocs.io/en/latest/frequently-asked-questions.html#what-is-the-memory-keyword-what-does-it-do
 * https://github.com/ethereum/go-ethereum/blob/master/core/vm/memory.go
 */
class Memory private(private val underlying: ByteString) {

  import Memory.zeros

  def store(offset: UInt256, b: Byte): Memory = store(offset, ByteString(b))

  def store(offset: UInt256, uint: UInt256): Memory = store(offset, uint.bytes)

  def store(offset: UInt256, bytes: Array[Byte]): Memory = store(offset, ByteString(bytes))

  /** Stores data at the given offset.
    * The memory is automatically expanded to accommodate new data - filling empty regions with zeroes if necessary -
    * hence an OOM error may be thrown.
    */
  def store(offset: UInt256, data: ByteString): Memory = {
    val idx: Int = offset.toInt

    val newUnderlying: ByteString =
      if (data.isEmpty)
        underlying
      else
        underlying.take(idx).padTo(idx, 0.toByte) ++ data ++ underlying.drop(idx + data.length)

    new Memory(newUnderlying)
  }

  def load(offset: UInt256): (UInt256, Memory) = {
    doLoad(offset, UInt256.Size) match {
      case (bs, memory) => (UInt256(bs), memory)
    }
  }

  def load(offset: UInt256, size: UInt256): (ByteString, Memory) = doLoad(offset, size.toInt)

  /** Returns a ByteString of a given size starting at the given offset of the Memory.
    * The memory is automatically expanded (with zeroes) when reading previously uninitialised regions,
    * hence an OOM error may be thrown.
    */
  private def doLoad(offset: UInt256, size: Int): (ByteString, Memory) =
    if (size <= 0)
      (ByteString.empty, this)
    else {
      val start: Int = offset.toInt
      val end: Int = start + size

      val newUnderlying = if (end <= underlying.size)
        underlying
      else
        underlying ++ zeros(end - underlying.size)

      (newUnderlying.slice(start, end), new Memory(newUnderlying))
    }

  /**
    * @return memory size in bytes
    */
  def size: Int = underlying.size

  override def equals(that: Any): Boolean = {
    that match {
      case that: Memory => this.underlying.equals(that.underlying)
      case other => false
    }
  }

  override def hashCode: Int = underlying.hashCode()

  override def toString: String = underlying.toString.replace("ByteString", this.getClass.getSimpleName)

}
