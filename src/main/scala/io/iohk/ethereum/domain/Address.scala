package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.vm.DataWord
import io.iohk.ethereum.utils.ByteUtils.padLeft
import org.spongycastle.util.encoders.Hex

object Address {

  val Length = 20

  def apply(bytes: ByteString): Address = {
    val truncated = bytes.takeRight(Length)
    val extended = padLeft(truncated, Length)
    new Address(extended)
  }

  def apply(dw: DataWord): Address = Address(dw.bytes)

  def apply(arr: Array[Byte]): Address = Address(ByteString(arr))

  def apply(addr: Long): Address = Address(DataWord(addr))

  val empty: Address = Address(ByteString())
}

class Address private(val bytes: ByteString) {

  def isEmpty: Boolean = bytes.isEmpty

  def toArray: Array[Byte] = bytes.toArray

  override def equals(that: Any): Boolean = that match {
    case addr: Address => addr.bytes == bytes
    case other => false
  }

  override def hashCode: Int =
    bytes.hashCode

  override def toString: String =
    s"0x${Hex.toHexString(toArray)}"

}
