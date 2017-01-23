package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.vm.DataWord._


class DataWordSpec extends FunSuite with PropertyChecks with ObjectGenerators {

  def bigIntToDataWord(n: BigInt): DataWord = {
    DataWord(ByteString(fixBigInt(n).toByteArray.takeRight(MaxLength)))
  }

  val specialNumbers = Array(BigInt(-1), BigInt(0), BigInt(1), MaxWord, -MaxWord, -MaxWord + 1)

  val pairs: Array[(BigInt, BigInt)] = specialNumbers
    .combinations(2)
    .map {case Array(n1, n2) => n1 -> n2}
    .toArray

  val specialCases = Table(("n1", "n2"), pairs: _*)

  /** For each operation (op) tests check a following property:
      For two BigInts (n1, n2):
      DataWord(n1) op DataWord(n2) == DataWord(n1 op n2)
   */
  test("&") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((bigIntToDataWord(n1) & bigIntToDataWord(n2)) == bigIntToDataWord(fixBigInt(n1) & fixBigInt(n2)))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((bigIntToDataWord(n1) & bigIntToDataWord(n2)) == bigIntToDataWord(fixBigInt(n1) & fixBigInt(n2)))
    }
  }

  test("|") {
   forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((bigIntToDataWord(n1) | bigIntToDataWord(n2)) == bigIntToDataWord(fixBigInt(n1) | fixBigInt(n2)))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((bigIntToDataWord(n1) | bigIntToDataWord(n2)) == bigIntToDataWord(fixBigInt(n1) | fixBigInt(n2)))
    }
  }

  test("^") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((bigIntToDataWord(n1) ^ bigIntToDataWord(n2)) == bigIntToDataWord(fixBigInt(n1) ^ fixBigInt(n2)))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((bigIntToDataWord(n1) ^ bigIntToDataWord(n2)) == bigIntToDataWord(fixBigInt(n1) ^ fixBigInt(n2)))
    }
  }

  test("negation") {
    forAll(bigIntGen) {(n: BigInt) =>
      assert(-bigIntToDataWord(n) == bigIntToDataWord(-n))
    }
    forAll(Table(("n"), specialNumbers: _*)) {(n: BigInt) =>
      assert(-bigIntToDataWord(n) == bigIntToDataWord(-n))
    }
    assert(-bigIntToDataWord(BigInt(1)) == bigIntToDataWord(BigInt(-1)))
  }

  test("+") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(bigIntToDataWord(n1) + bigIntToDataWord(n2) == bigIntToDataWord(n1 + n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(bigIntToDataWord(n1) + bigIntToDataWord(n2) == bigIntToDataWord(n1 + n2))
    }
  }

  test("-") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(bigIntToDataWord(n1) - bigIntToDataWord(n2) == bigIntToDataWord(n1 - n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(bigIntToDataWord(n1) - bigIntToDataWord(n2) == bigIntToDataWord(n1 - n2))
    }
  }

  test("*") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(bigIntToDataWord(n1) * bigIntToDataWord(n2) == bigIntToDataWord(n1 * n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(bigIntToDataWord(n1) * bigIntToDataWord(n2) == bigIntToDataWord(n1 * n2))
    }
  }

  test("/") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert(bigIntToDataWord(n1) / bigIntToDataWord(n2) == bigIntToDataWord(n1 / n2))
      }
    }
    assertThrows[ArithmeticException] {
      bigIntToDataWord(1) / bigIntToDataWord(0)
    }
  }

  test("Passing too long ByteString should throw an exception") {
    assertThrows[IllegalArgumentException] {
      DataWord(ByteString(Array.fill(MaxLength + 1)(1.toByte)))
    }
  }

}
