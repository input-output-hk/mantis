package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary,Gen}


class MemorySpec extends FunSuite with PropertyChecks {

  def zeros(size: Int): ByteString = {
    if (size <= 0)
      ByteString()
    else
      ByteString(Array.fill[Byte](size)(0))
  }

  def consecutiveBytes(size: Int, start: Int = 0): ByteString = {
    if (size <= 0)
      ByteString()
    else
      ByteString((start until (start + size)).map(_.toByte): _*)
  }

  import Arbitrary._
  import Gen._

  test("Store a Byte") {
    forAll(choose(10, 100), arbitrary[Byte], choose(0, 200)) {
      (initialMemorySize, b, idx) =>
      // We need this additional check.
      // Otherwise ScalaCheck generates negative numbers during shrinking.
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = new Memory(zeros(initialMemorySize)).store(UInt256(idx), b)
        val expectedSize = math.max(initialMemorySize, idx + 1)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(expectedSize).updated(idx, b))
      }
    }
  }

  test("Store an UInt256") {
    forAll(choose(10, 100), getUInt256Gen(), choose(0, 200)) {
      (initialMemorySize, uint, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = new Memory(zeros(initialMemorySize)).store(UInt256(idx), uint)
        val expectedSize = math.max(initialMemorySize, idx + UInt256.Size)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(idx) ++ uint.bytes ++ zeros(memory.size - idx - UInt256.Size))
      }
    }
  }

  test("Store an Array[Byte]") {
    forAll(choose(10, 100), randomSizeByteArrayGen(0, 100), choose(0, 200)) {
      (initialMemorySize, arr, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = new Memory(zeros(initialMemorySize)).store(UInt256(idx), arr)
        val expectedSize = math.max(initialMemorySize, idx + arr.length)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - arr.length))
      }
    }
    // regression
    val initialMemorySize = 70
    val arr = Array[Byte]()
    val idx = 134
    val memory = new Memory(zeros(initialMemorySize)).store(UInt256(idx), arr)
    val expectedSize = math.max(initialMemorySize, idx + arr.length)
    assert(memory.size == expectedSize)
    assert(memory.underlying == zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - arr.length))
  }

  test("Store a ByteString") {
    forAll(choose(10, 100), randomSizeByteArrayGen(0, 100), choose(0, 200)) {
      (initialMemorySize, arr, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val bs =  ByteString(arr)
        val memory = new Memory(zeros(initialMemorySize)).store(UInt256(idx), bs)
        val expectedSize = math.max(initialMemorySize, idx + bs.size)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - bs.size))
      }
    }
  }

  test("Load an UInt256") {
    forAll(choose(0, 100), choose(0, 200)) {
      (initialMemorySize, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val initialMemory = new Memory(consecutiveBytes(initialMemorySize))
        val addr = UInt256(idx)
        val (uint, memory) = initialMemory.load(addr)
        val expectedMemorySize = math.max(initialMemorySize, idx + UInt256.Size)
        assert(memory.size == expectedMemorySize)
        assert(memory.underlying == consecutiveBytes(initialMemorySize) ++ zeros(expectedMemorySize - initialMemorySize))
        if (idx >= initialMemorySize)
          assert(uint.bytes == zeros(UInt256.Size))
        else if (idx + UInt256.Size > initialMemorySize)
          assert(uint.bytes == (consecutiveBytes(initialMemorySize - idx, idx) ++ zeros(idx + UInt256.Size - initialMemorySize)))
        else
          assert(uint.bytes == consecutiveBytes(UInt256.Size, idx))
      }
    }
  }

  test("Load a ByteString") {
    forAll(choose(0, 100), choose(0, 200), choose(1, 100)) {
      (initialMemorySize, idx, size) =>
      whenever(initialMemorySize >= 0 && idx >= 0 && size > 0) {
        val initialMemory = new Memory(consecutiveBytes(initialMemorySize))
        val (bs, memory) = initialMemory.load(UInt256(idx), UInt256(size))
        val expectedMemorySize = math.max(initialMemorySize, idx + size)
        assert(memory.size == expectedMemorySize)
        assert(memory.underlying == consecutiveBytes(initialMemorySize) ++ zeros(expectedMemorySize - initialMemorySize))
        if (idx >= initialMemorySize)
          assert(bs == zeros(size))
        else if (idx + size > initialMemorySize)
          assert(bs == (consecutiveBytes(initialMemorySize - idx, idx) ++ zeros(idx + size - initialMemorySize)))
        else
          assert(bs == consecutiveBytes(size, idx))
      }
    }
  }

}
