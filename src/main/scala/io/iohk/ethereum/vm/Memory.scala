package io.iohk.ethereum.vm

import akka.util.ByteString
import cats.syntax.either._

/**
 * Volatile memory with 256 bit address space.
 * Every mutating operation on a Memory returns a new updated copy of it.
 *
 * Related reading:
 * https://solidity.readthedocs.io/en/latest/frequently-asked-questions.html#what-is-the-memory-keyword-what-does-it-do
 * https://github.com/ethereum/go-ethereum/blob/master/core/vm/memory.go
 */
class Memory(val underlying: ByteString = ByteString()) {

  def store(addr: DataWord, b: Byte): Memory = store(addr, ByteString(b))

  def store(addr: DataWord, dw: DataWord): Memory = store(addr, dw.bytes)

  def store(addr: DataWord, bytes: Array[Byte]): Memory = store(addr, ByteString(bytes))

  /** Stores a ByteString under the given address.
   * Underlying byte array is expanded if a ByteString doesn't fit into it.
   * All empty cells of an expanded array are set to 0.
   * This method may throw OOM.
   */
  def store(addr: DataWord, bs: ByteString): Memory = {
    val idx: Int = addr.intValue
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

  def load(addr: DataWord): (DataWord, Memory) = {
    doLoad(addr, DataWord.MaxLength) match {
      case (bs, memory) => DataWord(bs) -> memory
    }
  }

  def load(addr: DataWord, size: DataWord): (ByteString, Memory) = doLoad(addr, size.intValue)

  /** Returns a ByteString of a given size starting at the given address of the Memory.
   * Underlying byte array is expanded and filled with 0's if addr + size exceeds size
   * of the memory.
   * This method may throw OOM
   */
  private def doLoad(addr: DataWord, size: Int): (ByteString, Memory) = {
    val start: Int = addr.intValue
    val end: Int = start + size
    val newUnderlying = if (end <= underlying.size)
      underlying
    else
      underlying ++ zeros(end - underlying.size)
    newUnderlying.slice(start, end) -> new Memory(newUnderlying)
  }

  def size: Int = underlying.size

  private def zeros(size: Int): ByteString = {
    ByteString(Array.fill[Byte](size)(0))
  }

}
