package io.iohk.ethereum.vm

import akka.util.ByteString
import cats.syntax.either._

/**
 Related reading:
 https://solidity.readthedocs.io/en/latest/frequently-asked-questions.html#what-is-the-memory-keyword-what-does-it-do
 https://github.com/ethereum/go-ethereum/blob/master/core/vm/memory.go
 */
case class Memory(buffer: ByteString = ByteString(), error: Option[MemoryError] = None) {

  import DataWord.{MaxLength, Zeros}

  def store(addr: Int, dw: DataWord): Memory = storeByteBuffer(addr, dw.value)

  def store(addr: Int, value: Byte): Memory = storeByteBuffer(addr, ByteString(value))

  def load(addr: Int): DataWord = {
    if (addr < 0) {
      DataWord(Zeros)
    } else {
      DataWord(buffer.drop(addr).take(MaxLength))
    }
  }

  def size(): Int = buffer.size

  def withBuffer(buffer: ByteString): Memory = copy(buffer = buffer)

  def withError(error: MemoryError): Memory = copy(error = Some(error))

  private def storeByteBuffer(addr: Int, value: ByteString): Memory = {
    val updatedState = for {
      _ <- error.toLeft(buffer)
      newBuf <- newBuffer(addr, value)
    } yield {
      this.withBuffer(newBuf)
    }
    updatedState.valueOr(this.withError)
  }

  private def newBuffer(addr: Int, value: ByteString): Either[MemoryError, ByteString] = {
    if (addr < 0) {
      InvalidAddress.asLeft
    } else {
      // new buffer fits into old buffer
      val newBuffer = if (addr + value.length <= buffer.length) {
        val (prepending, following) = buffer.splitAt(addr)
        prepending ++ value ++ following.drop(value.length)
        // new buffer partially fits into old buffer
      } else if (addr <= buffer.length) {
        buffer.take(addr) ++ value
        // there is a gap (possibly empty) between old buffer and new buffer
      } else {
        buffer ++ zeros(addr - buffer.length) ++ value
      }
      newBuffer.asRight
    }
  }

  private def zeros(length: Int): ByteString = {
    ByteString(Array.fill[Byte](length)(0))
  }

}
