package io.iohk.ethereum.mpt

import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class HexPrefixSuite extends FunSuite
  with PropertyChecks
  with ObjectGenerators {

  test("HexPrefix encoding"){
    forAll(hexPrefixDecodeParametersGen()) { pair: (Array[Byte], Boolean) =>
      val (bytes, t) = pair
      val nibbles = HexPrefix.bytesToNibbles(bytes = bytes)
      val packed = HexPrefix.encode(nibbles, t)
      val (unpacked, b) = HexPrefix.decode(packed)
      val bytesObtained = HexPrefix.nibblesToBytes(unpacked)
      assert(bytes sameElements bytesObtained)
      assert(b==t)
    }
  }

  test("testCompactEncodeOddCompact"){
    val test: Array[Byte] = Array[Byte](1, 2, 3, 4, 5)
    val expectedData: Array[Byte] = Array[Byte](0x11, 0x23, 0x45)
    assert(expectedData sameElements HexPrefix.encode(test, isLeaf = false))
  }

  test("testCompactEncodeEvenCompact"){
    val test: Array[Byte] = Array[Byte](0, 1, 2, 3, 4, 5)
    val expectedData: Array[Byte] = Array[Byte](0x00, 0x01, 0x23, 0x45)
    assert(expectedData sameElements HexPrefix.encode(test, isLeaf = false))
  }

  test("testCompactEncodeEvenTerminated"){
    val test: Array[Byte] = Array[Byte](0, 15, 1, 12, 11, 8)
    val expectedData: Array[Byte] = Array[Byte](0x20, 0x0f, 0x1c, 0xb8.toByte)
    assert(expectedData sameElements HexPrefix.encode(test, isLeaf = true))
  }

  test("testCompactEncodeOddTerminated"){
    val test: Array[Byte] = Array[Byte](15, 1, 12, 11, 8)
    val expectedData: Array[Byte] = Array[Byte](0x3f, 0x1c, 0xb8.toByte)
    assert(expectedData sameElements HexPrefix.encode(test, isLeaf = true))
  }

  test("testCompactDecodeOddCompact"){
    val test: Array[Byte] = Array[Byte](0x11, 0x23, 0x45)
    val expected: Array[Byte] = Array[Byte](1, 2, 3, 4, 5)
    val (obtained, t) = HexPrefix.decode(test)
    assert(expected sameElements obtained)
    assert(!t)
  }

  test("testCompactDecodeEvenCompact"){
    val test: Array[Byte] = Array[Byte](0x00, 0x01, 0x23, 0x45)
    val expected: Array[Byte] = Array[Byte](0, 1, 2, 3, 4, 5)
    val (obtained, t) = HexPrefix.decode(test)
    assert(expected sameElements obtained)
    assert(!t)
  }

  test("testCompactDecodeEvenTerminated"){
    val test: Array[Byte] = Array[Byte](0x20, 0x0f, 0x1c, 0xb8.toByte)
    val expected: Array[Byte] = Array[Byte](0, 15, 1, 12, 11, 8)
    val (obtained, t) = HexPrefix.decode(test)
    assert(expected sameElements obtained)
    assert(t)
  }

  test("testCompactDecodeOddTerminated"){
    val test: Array[Byte] = Array[Byte](0x3f, 0x1c, 0xb8.toByte)
    val expected: Array[Byte] = Array[Byte](15, 1, 12, 11, 8)
    val (obtained, t) = HexPrefix.decode(test)
    assert(expected sameElements obtained)
    assert(t)
  }

  test("testCompactHexEncode_1"){
    val test: Array[Byte] = "stallion".getBytes
    val result: Array[Byte] = Array[Byte](7, 3, 7, 4, 6, 1, 6, 12, 6, 12, 6, 9, 6, 15, 6, 14)
    assert(result sameElements HexPrefix.bytesToNibbles(bytes = test))
  }

  test("testCompactHexEncode_2"){
    val test: Array[Byte] = "verb".getBytes
    val result: Array[Byte] = Array[Byte](7, 6, 6, 5, 7, 2, 6, 2)
    assert(result sameElements HexPrefix.bytesToNibbles(bytes = test))
  }

  test("testCompactHexEncode_3"){
    val test: Array[Byte] = "puppy".getBytes
    val result: Array[Byte] = Array[Byte](7, 0, 7, 5, 7, 0, 7, 0, 7, 9)
    assert(result sameElements HexPrefix.bytesToNibbles(bytes = test))
  }
}
