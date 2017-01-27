package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary,Gen}


class MemorySpec extends FunSuite with PropertyChecks with ObjectGenerators {

  def zeros(size: Int): ByteString = {
    if (size <= 0)
      ByteString()
    else
      ByteString(Array.fill[Byte](size)(0))
  }

  def nonNegativeInts(size: Int, start: Int = 0): ByteString = {
    if (size <= 0)
      ByteString()
    else
      ByteString((start until (start + size)).map(_.toByte).toArray)
  }

  import Arbitrary._
  import Gen._

  test("Store a Byte") {
    forAll(choose(10, 100), arbitrary[Byte], choose(0, 200)) {
      (initialMemorySize, b, idx) =>
      // We need this additional check.
      // Otherwise ScalaCheck generates negative numbers during shrinking
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = new Memory(zeros(initialMemorySize)).store(DataWord(idx), b)
        val expectedSize = math.max(initialMemorySize, idx + 1)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(expectedSize).updated(idx, b))
      }
    }
  }

  test("Store a DataWord") {
    forAll(choose(10, 100), dataWordGen, choose(0, 200)) {
      (initialMemorySize, dw, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = new Memory(zeros(initialMemorySize)).store(DataWord(idx), dw)
        val expectedSize = math.max(initialMemorySize, idx + DataWord.MaxLength)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(idx) ++ dw.bytes ++ zeros(memory.size - idx - DataWord.MaxLength))
      }
    }
  }

  test("Store an Array[Byte]") {
    forAll(choose(10, 100), randomSizeByteArrayGen(0, 100), choose(0, 200)) {
      (initialMemorySize, arr, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = new Memory(zeros(initialMemorySize)).store(DataWord(idx), arr)
        val expectedSize = math.max(initialMemorySize, idx + arr.size)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - arr.size))
      }
    }
    // regression
    val initialMemorySize = 70
    val arr = Array[Byte]()
    val idx = 134
    val memory = new Memory(zeros(initialMemorySize)).store(DataWord(idx), arr)
    val expectedSize = math.max(initialMemorySize, idx + arr.size)
    assert(memory.size == expectedSize)
    assert(memory.underlying == zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - arr.size))
  }

  test("Store a ByteString") {
    forAll(choose(10, 100), randomSizeByteArrayGen(0, 100), choose(0, 200)) {
      (initialMemorySize, arr, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val bs =  ByteString(arr)
        val memory = new Memory(zeros(initialMemorySize)).store(DataWord(idx), bs)
        val expectedSize = math.max(initialMemorySize, idx + bs.size)
        assert(memory.size == expectedSize)
        assert(memory.underlying == zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - bs.size))
      }
    }
  }

  test("Load a DataWord") {
    forAll(choose(0, 100), choose(0, 200)) {
      (initialMemorySize, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val initialMemory = new Memory(nonNegativeInts(initialMemorySize))
        val addr = DataWord(idx)
        val (dw, memory) = initialMemory.load(addr)
        val expectedMemorySize = math.max(initialMemorySize, idx + DataWord.MaxLength)
        assert(memory.size == expectedMemorySize)
        assert(memory.underlying == nonNegativeInts(initialMemorySize) ++ zeros(expectedMemorySize - initialMemorySize))
        if (idx >= initialMemorySize)
          assert(dw.bytes == zeros(DataWord.MaxLength))
        else if (idx + DataWord.MaxLength > initialMemorySize)
          assert(dw.bytes == (nonNegativeInts(initialMemorySize - idx, idx) ++ zeros(idx + DataWord.MaxLength - initialMemorySize)))
        else
          assert(dw.bytes == nonNegativeInts(DataWord.MaxLength, idx))
      }
    }
  }

  test("Load a ByteString") {
    forAll(choose(0, 100), choose(0, 200), choose(1, 100)) {
      (initialMemorySize, idx, size) =>
      whenever(initialMemorySize >= 0 && idx >= 0 && size > 0) {
        val initialMemory = new Memory(nonNegativeInts(initialMemorySize))
        val (bs, memory) = initialMemory.load(DataWord(idx), DataWord(size))
        val expectedMemorySize = math.max(initialMemorySize, idx + size)
        assert(memory.size == expectedMemorySize)
        assert(memory.underlying == nonNegativeInts(initialMemorySize) ++ zeros(expectedMemorySize - initialMemorySize))
        if (idx >= initialMemorySize)
          assert(bs == zeros(size))
        else if (idx + size > initialMemorySize)
          assert(bs == (nonNegativeInts(initialMemorySize - idx, idx) ++ zeros(idx + size - initialMemorySize)))
        else
          assert(bs == nonNegativeInts(size, idx))
      }
    }
  }

}
