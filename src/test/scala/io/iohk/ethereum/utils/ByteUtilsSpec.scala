package io.iohk.ethereum.utils

import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ByteUtilsSpec extends FunSuite with ScalaCheckPropertyChecks with ObjectGenerators {
  test("Convert Bytes to Int in little endian") {
    forAll(byteArrayOfNItemsGen(32)) { bytes =>
      val toInts = ByteUtils.bytesToInts(bytes, bigEndian = false)
      val asBytes = ByteUtils.intsToBytes(toInts, bigEndian = false)
      assert(asBytes sameElements bytes)
    }
  }

  test("Convert Bytes to Int in big endian") {
    forAll(byteArrayOfNItemsGen(32)) { bytes =>
      val toInts = ByteUtils.bytesToInts(bytes, bigEndian = true)
      val asBytes = ByteUtils.intsToBytes(toInts, bigEndian = true)
      assert(asBytes sameElements bytes)
    }
  }
}
