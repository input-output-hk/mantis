package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.vm.DataWord._


class MemorySpec extends FunSuite with PropertyChecks with ObjectGenerators {

  import DataWord._

  test("store DataWord") {
    assert(Memory().store(0, fill(1)).buffer == fill(1).bytes)
    assert(Memory().store(2, fill(1)).buffer == ByteString(0, 0) ++ fill(1).bytes)
    assert(Memory().store(2, fill(1)).store(1, fill(2)).buffer == ByteString(0) ++ fill(2).bytes :+ 1)
    assert(Memory().store(-1, fill(1)).error == Some(InvalidAddress))
  }

  test("store Byte") {
    assert(Memory().store(0, 1.toByte).buffer == ByteString(1))
    assert(Memory().store(2, 1.toByte).buffer == ByteString(0, 0, 1))
    assert(Memory().store(2, 1.toByte).store(1, 2.toByte).buffer == ByteString(0, 2, 1))
    assert(Memory().store(-1, 1.toByte).error == Some(InvalidAddress))
  }

  test("load DataWord") {
    // assert(Memory(ByteString(0, 0) ++ fill(1).value).load(-1).value == fill(0).value)
    // assert(Memory(ByteString(0, 0) ++ fill(1).value).load(1).value == 0 +: ByteString(Array.fill[Byte](31)(1)))
    assert(Memory(ByteString(0, 0) ++ fill(1).bytes).load(3).bytes == 0 +: ByteString(Array.fill[Byte](31)(1)))
  }

  def fill(b: Byte): DataWord =
    DataWord(ByteString(Array.fill[Byte](MaxLength)(b)))

}
