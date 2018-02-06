package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.UInt256

object MockStorage {
  val Empty = MockStorage()

  def fromSeq(words: Seq[UInt256]): MockStorage = {
    val map = words.zipWithIndex.map { case (w, i) => UInt256(i).bytes -> w.bytes }.toMap
    MockStorage(map)
  }
}

case class MockStorage(data: Map[ByteString, ByteString] = Map()) extends Storage[MockStorage] {
  def store(addr: ByteString, value: ByteString): MockStorage = {
    val updated =
      if (UInt256(value) == UInt256.Zero)
        data - addr
      else
        data + (addr -> value)

    copy(data = updated)
  }

  def load(addr: ByteString): ByteString =
    data.getOrElse(addr, UInt256.Zero.bytes)

  def isEmpty: Boolean =
    data.isEmpty
}
