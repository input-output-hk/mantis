package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}


class MemorySpec extends FunSuite with PropertyChecks with Matchers {

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
        val memory = Memory.empty.store(0, zeros(initialMemorySize)).store(idx, b)

        val expectedSize = math.max(initialMemorySize, idx + 1)
        val expectedContents = zeros(expectedSize).updated(idx, b)

        memory.size shouldEqual expectedSize
        memory.load(0, memory.size)._1 shouldEqual expectedContents
      }
    }
  }

  test("Store an UInt256") {
    forAll(choose(10, 100), getUInt256Gen(), choose(0, 200)) {
      (initialMemorySize, uint, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = Memory.empty.store(0, zeros(initialMemorySize)).store(idx, uint)

        val expectedSize = math.max(initialMemorySize, idx + UInt256.Size)
        val expectedContents = zeros(idx) ++ uint.bytes ++ zeros(memory.size - idx - UInt256.Size)

        memory.size shouldEqual expectedSize
        memory.load(0, memory.size)._1 shouldEqual expectedContents
      }
    }
  }

  test("Store an Array[Byte]") {
    forAll(choose(10, 100), randomSizeByteArrayGen(0, 100), choose(0, 200)) {
      (initialMemorySize, arr, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val memory = Memory.empty.store(0, zeros(initialMemorySize)).store(idx, arr)

        val requiredSize = if (arr.length == 0) 0 else idx + arr.length
        val expectedSize = math.max(initialMemorySize, requiredSize)
        val expectedContents =
          if (arr.length == 0)
            zeros(initialMemorySize)
          else
            zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - arr.length)

        memory.size shouldEqual expectedSize
        memory.load(0, memory.size)._1 shouldEqual expectedContents
      }
    }
  }

  test("Store a ByteString") {
    forAll(choose(10, 100), randomSizeByteArrayGen(0, 100), choose(0, 200)) {
      (initialMemorySize, arr, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val bs =  ByteString(arr)
        val memory = Memory.empty.store(0, zeros(initialMemorySize)).store(idx, bs)

        val requiredSize = if (bs.isEmpty) 0 else idx + bs.length
        val expectedSize = math.max(initialMemorySize, requiredSize)
        val expectedContents =
          if (bs.isEmpty)
            zeros(initialMemorySize)
          else
            zeros(idx) ++ ByteString(arr) ++ zeros(memory.size - idx - bs.size)

        memory.size shouldEqual expectedSize
        memory.load(0, memory.size)._1 shouldEqual expectedContents
      }
    }
  }

  test("Load an UInt256") {
    forAll(choose(0, 100), choose(0, 200)) {
      (initialMemorySize, idx) =>
      whenever(initialMemorySize >= 0 && idx >= 0) {
        val initialMemory = Memory.empty.store(0, consecutiveBytes(initialMemorySize))
        val (uint, memory) = initialMemory.load(idx)

        val expectedMemorySize = math.max(initialMemorySize, idx + UInt256.Size)
        val expectedContents = consecutiveBytes(initialMemorySize) ++ zeros(expectedMemorySize - initialMemorySize)
        val expectedResult = UInt256(
          if (idx >= initialMemorySize)
            zeros(UInt256.Size)
          else if (idx + UInt256.Size > initialMemorySize)
            consecutiveBytes(initialMemorySize - idx, idx) ++ zeros(idx + UInt256.Size - initialMemorySize)
          else
            consecutiveBytes(UInt256.Size, idx)
        )

        memory.size shouldEqual expectedMemorySize
        memory.load(0, memory.size)._1 shouldEqual expectedContents
        uint shouldEqual expectedResult
      }
    }
  }

  test("Load a ByteString") {
    forAll(choose(0, 100), choose(0, 200), choose(1, 100)) {
      (initialMemorySize, idx, size) =>
      whenever(initialMemorySize >= 0 && idx >= 0 && size > 0) {
        val initialMemory = Memory.empty.store(0, consecutiveBytes(initialMemorySize))
        val (bs, memory) = initialMemory.load(idx, size)

        val requiredSize = if (size == 0) 0 else idx + size
        val expectedMemorySize = math.max(initialMemorySize, requiredSize)
        val expectedContents = consecutiveBytes(initialMemorySize) ++ zeros(expectedMemorySize - initialMemorySize)
        val expectedResult =
          if (idx >= initialMemorySize)
            zeros(size)
          else if (idx + size > initialMemorySize)
            consecutiveBytes(initialMemorySize - idx, idx) ++ zeros(idx + size - initialMemorySize)
          else
            consecutiveBytes(size, idx)

        memory.size shouldEqual expectedMemorySize
        memory.load(0, memory.size)._1 shouldEqual expectedContents
        bs shouldEqual expectedResult
      }
    }
  }

  test("Correctly increase memory size when storing") {

    val table = Table(
      ("initialSize", "offset", "dataSize", "expectedDelta"),
      (0, 0, 1, 1),
      (0, 0, 32, 32),
      (0, 32, 31, 63),
      (64, 32, 64, 32),
      (64, 32, 16, 0),
      (64, 96, 0, 0),
      (0, 32, 0, 0)
    )

    forAll(table) { (initialSize, offset, dataSize, expectedDelta) =>
      val initMem = Memory.empty.store(0, zeros(initialSize))
      val updatedMem = initMem.store(offset, consecutiveBytes(dataSize))
      (updatedMem.size - initMem.size) shouldEqual expectedDelta
    }

  }

  test("Correctly increase memory size when loading") {

    val table = Table(
      ("initialSize", "offset", "dataSize", "expectedDelta"),
      (0, 0, 1, 1),
      (0, 0, 32, 32),
      (0, 32, 31, 63),
      (64, 32, 64, 32),
      (64, 32, 16, 0),
      (64, 96, 0, 0),
      (0, 32, 0, 0)
    )

    forAll(table) { (initialSize, offset, dataSize, expectedDelta) =>
      val initMem = Memory.empty.store(0, zeros(initialSize))
      val updatedMem = initMem.load(offset, dataSize)._2
      (updatedMem.size - initMem.size) shouldEqual expectedDelta
    }
  }
}
