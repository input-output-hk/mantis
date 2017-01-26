package io.iohk.ethereum.vm

import akka.util.ByteString
import cats.syntax.either._

/**
 Related reading:
 https://solidity.readthedocs.io/en/latest/frequently-asked-questions.html#what-is-the-memory-keyword-what-does-it-do
 https://github.com/ethereum/go-ethereum/blob/master/core/vm/memory.go
 */
class Memory(val underlying: ByteString = ByteString()) {

  def store(addr: DataWord, b: Byte): Memory = store(addr, ByteString(b))

  def store(addr: DataWord, dw: DataWord): Memory = store(addr, dw.bytes)

  def store(addr: DataWord, bytes: Array[Byte]): Memory = store(addr, ByteString(bytes))

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

  private def doLoad(addr: DataWord, size: Int): (ByteString, Memory) = {
    val start: Int = addr.intValue
    val end: Int = start + size
    println(s">>> START: $start END: $end")
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
