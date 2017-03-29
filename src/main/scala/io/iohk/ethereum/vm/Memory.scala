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
class Memory(private[vm] val underlying: ByteString) {

  import Memory.zeros

  def store(offset: UInt256, b: Byte): Memory = store(offset, ByteString(b))

  def store(offset: UInt256, dw: UInt256): Memory = store(offset, dw.bytes)

  def store(offset: UInt256, bytes: Array[Byte]): Memory = store(offset, ByteString(bytes))

  /** Stores a ByteString under the given address.
   * Underlying byte array is expanded if a ByteString doesn't fit into it.
   * All empty cells of an expanded array are set to 0.
   * This method may throw OOM.
   */
  def store(offset: UInt256, bs: ByteString): Memory = {
    val idx: Int = offset.toInt
    val newUnderlying: ByteString = if (idx + bs.length <= underlying.length) {
      // a new buffer fits into an old buffer
      val (prepending, following) = underlying.splitAt(idx)
      prepending ++ bs ++ following.drop(bs.length)
    } else if (idx <= underlying.length) {
      // a new buffer partially fits into an old buffer
      underlying.take(idx) ++ bs
    } else {
      // there is a gap (possibly empty) between an old buffer and a new buffer
      underlying ++ zeros(idx - underlying.length) ++ bs
    }
    new Memory(newUnderlying)
  }

  def load(offset: UInt256): (UInt256, Memory) = {
    doLoad(offset, UInt256.Size) match {
      case (bs, memory) => UInt256(bs) -> memory
    }
  }

  def load(offset: UInt256, size: UInt256): (ByteString, Memory) = doLoad(offset, size.toInt)

  /** Returns a ByteString of a given size starting at the given address of the Memory.
   * Underlying byte array is expanded and filled with 0's if addr + size exceeds size
   * of the memory.
   * This method may throw OOM.
   */
  private def doLoad(offset: UInt256, size: Int): (ByteString, Memory) = {
    val start: Int = offset.toInt
    val end: Int = start + size
    val newUnderlying = if (end <= underlying.size)
      underlying
    else
      underlying ++ zeros(end - underlying.size)
    newUnderlying.slice(start, end) -> new Memory(newUnderlying)
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
