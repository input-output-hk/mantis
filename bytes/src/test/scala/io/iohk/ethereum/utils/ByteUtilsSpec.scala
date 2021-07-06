package io.iohk.ethereum.utils

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ByteUtilsSpec extends AnyFunSuite with ScalaCheckPropertyChecks {
  def byteArrayOfNItemsGen(n: Int): Gen[Array[Byte]] =
    Gen.listOfN(n, Arbitrary.arbitrary[Byte]).map(_.toArray)

  test("Convert Bytes to Int in little endian") {
    forAll(byteArrayOfNItemsGen(32)) { bytes =>
      val toInts = ByteUtils.bytesToInts(bytes, bigEndian = false)
      val asBytes = ByteUtils.intsToBytes(toInts, bigEndian = false)
      assert(asBytes.sameElements(bytes))
    }
  }

  test("Convert Bytes to Int in big endian") {
    forAll(byteArrayOfNItemsGen(32)) { bytes =>
      val toInts = ByteUtils.bytesToInts(bytes, bigEndian = true)
      val asBytes = ByteUtils.intsToBytes(toInts, bigEndian = true)
      assert(asBytes.sameElements(bytes))
    }
  }
}
