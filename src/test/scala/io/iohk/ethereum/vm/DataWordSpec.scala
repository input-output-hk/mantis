package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.vm.DataWord._


class DataWordSpec extends FunSuite with PropertyChecks with ObjectGenerators {


  val specialNumbers = Array(BigInt(-1), BigInt(0), BigInt(1), MaxWord.toBigInt, -MaxWord.toBigInt, -MaxWord.toBigInt + 1)

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
      assert((DataWord(n1) & DataWord(n2)) == DataWord(n1 & n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((DataWord(n1) & DataWord(n2)) == DataWord(n1 & n2))
    }
  }

  test("|") {
   forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((DataWord(n1) | DataWord(n2)) == DataWord(n1 | n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((DataWord(n1) | DataWord(n2)) == DataWord(n1 | n2))
    }
  }

  test("^") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((DataWord(n1) ^ DataWord(n2)) == DataWord(n1 ^ n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((DataWord(n1) ^ DataWord(n2)) == DataWord(n1 ^ n2))
    }
  }

  test("~") {
    forAll(bigIntGen) { n: BigInt =>
      assert(~DataWord(n) == DataWord(~n))
    }
    forAll(Table("n", specialNumbers: _*)) { n: BigInt =>
      assert(~DataWord(n) == DataWord(~n))
    }
  }

  test("negation") {
    forAll(bigIntGen) {(n: BigInt) =>
      assert(-DataWord(n) == DataWord(-n))
    }
    forAll(Table(("n"), specialNumbers: _*)) {(n: BigInt) =>
      assert(-DataWord(n) == DataWord(-n))
    }
    assert(-DataWord(BigInt(1)) == DataWord(BigInt(-1)))
  }

  test("+") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(DataWord(n1) + DataWord(n2) == DataWord(n1 + n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(DataWord(n1) + DataWord(n2) == DataWord(n1 + n2))
    }
  }

  test("-") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(DataWord(n1) - DataWord(n2) == DataWord(n1 - n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(DataWord(n1) - DataWord(n2) == DataWord(n1 - n2))
    }
  }

  test("*") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(DataWord(n1) * DataWord(n2) == DataWord(n1 * n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(DataWord(n1) * DataWord(n2) == DataWord(n1 * n2))
    }
  }

  test("/") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert(DataWord(n1) / DataWord(n2) == DataWord(n1 / n2))
      }
    }
    assertThrows[ArithmeticException] {
      DataWord(1) / DataWord(0)
    }
  }

  test("intValue") {
    forAll(bigIntGen) { n: BigInt =>
      assert(DataWord(n).intValue == n.intValue)
    }
    forAll(Table("n", specialNumbers: _*)) { n: BigInt =>
      assert(DataWord(n).intValue == n.intValue)
    }
  }

  test("Passing too long ByteString should throw an exception") {
    assertThrows[IllegalArgumentException] {
      DataWord(ByteString(Array.fill(MaxLength + 1)(1.toByte)))
    }
  }

}
