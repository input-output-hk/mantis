package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.vm.DataWord

case object Address {

  val Length = 20

  def apply(dw: DataWord): Address = Address(dw.bytes.takeRight(Length))

  def apply(arr: Array[Byte]): Address = Address(ByteString(arr))

  val empty: Address = Address(ByteString())

}

case class Address(bytes: ByteString) {

  import Address.Length

  require(bytes.length == Length || bytes.isEmpty, s"Input ByteString has to have exactly $Length bytes or be empty.")

  def isEmpty: Boolean = bytes.isEmpty

  def toArray: Array[Byte] = bytes.toArray

}
