package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.vm.DataWord._


class DataWordSpec extends FunSuite with PropertyChecks {

  val specialNumbers = Array(BigInt(-1), BigInt(0), BigInt(1), MaxValue.toBigInt, -MaxValue.toBigInt, -MaxValue.toBigInt + 1)

  val pairs: Array[(BigInt, BigInt)] = specialNumbers
    .combinations(2)
    .map {case Array(n1, n2) => n1 -> n2}
    .toArray

  val specialCases = Table(("n1", "n2"), pairs: _*)

  def toSignedBigInt(n: BigInt): BigInt = if (n > MaxSignedValue) n - Modulus else n

  def toUnsignedBigInt(n: BigInt): BigInt = if (n < 0) n + Modulus else n

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
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      whenever(n1 > 0 && n2 > 0) {
        assert(DataWord(n1) / DataWord(n2) == DataWord(n1 / n2))
      }
    }
    assert(DataWord(1) / Zero == Zero)
  }

  test("sdiv") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        val expected: BigInt = toUnsignedBigInt(toSignedBigInt(n1) / toSignedBigInt(n2))
        assert((DataWord(n1) sdiv DataWord(n2)) == DataWord(expected))
      }
    }
    assert((DataWord(-1) sdiv DataWord(-MaxValue.toBigInt)) == DataWord(-1))
    assert((DataWord(-1) sdiv Zero) == Zero)
  }

  test("mod") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((DataWord(n1) mod DataWord(n2)) == DataWord(n1 mod n2))
      }
    }
    assert((DataWord(-1) mod DataWord(MaxValue.toBigInt)) == Zero)
    assert((DataWord(1) mod Zero) == Zero)
  }

  test("smod") {
    assert((DataWord(Modulus - 1) smod DataWord(3)) == DataWord(Modulus - 1))
    assert((DataWord(-1) smod DataWord(MaxValue.toBigInt)) == Zero)
    assert((DataWord(1) smod Zero) == Zero)
  }

  test("addmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(DataWord(n1).addmod(DataWord(n2), DataWord(n3)) == DataWord((n1 + n2) mod n3))
      }
    }
    assert(DataWord(42).addmod(DataWord(42), Zero) == Zero)
  }

  test("mulmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(DataWord(n1).mulmod(DataWord(n2), DataWord(n3)) == DataWord((n1 * n2) mod n3))
      }
    }
    assert(DataWord(42).mulmod(DataWord(42), Zero) == Zero)
  }

  test("**") {
    val TestModulus: BigInt = BigInt(2).pow(Size * 8)
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert(DataWord(n1) ** DataWord(n2) == DataWord(n1.modPow(n2, TestModulus)))
      }
    }
  }

  test("intValue") {
    assert(specialNumbers.map(DataWord(_).intValue).toSeq == Seq(Int.MaxValue, 0, 1, Int.MaxValue, 1, 2))
  }

  test("comparison") {
    type CFDW = (DataWord, DataWord) => Boolean
    type CFBI = (BigInt, BigInt) => Boolean
    case class Cmp(dw: CFDW, bi: CFBI)

    val cmpFuncDataWord = Seq[CFDW](_ > _, _ >= _, _ < _, _ <= _)
    val cmpFuncBigInt   = Seq[CFBI](_ > _, _ >= _, _ < _, _ <= _)
    val comparators = cmpFuncDataWord.zip(cmpFuncBigInt).map(Cmp.tupled)

    val dataWordGen = bigIntGen.map(DataWord(_))

    forAll(Table("comparators", comparators: _*)) { cmp =>
      forAll(dataWordGen, dataWordGen) { (a, b) =>
        assert(cmp.dw(a, b) == cmp.bi(a.toBigInt, b.toBigInt))
      }

      forAll(specialCases) { (x, y) =>
        val (a, b) = (DataWord(x), DataWord(y))
        assert(cmp.dw(a, b) == cmp.bi(a.toBigInt, b.toBigInt))
      }
    }
  }

  test("Passing too long ByteString should throw an exception") {
    assertThrows[IllegalArgumentException] {
      DataWord(ByteString(Array.fill(Size + 1)(1.toByte)))
    }
  }

  test("DataWord converted to a byte array should always have length 32 bytes") {
    forAll(bigIntGen) { n =>
      assert(DataWord(n).bytes.size == 32)
    }
    // regression
    assert(DataWord(BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935")).bytes.size == 32)
  }

  test("2-way bytes conversion") {
    forAll(getDataWordGen()) { x =>
      val y = DataWord(x.bytes)
      assert(x === y)
    }

    forAll(getByteStringGen(0, 32)) { xs =>
      val ys = DataWord(xs).bytes
      assert(xs.dropWhile(_ == 0) === ys.dropWhile(_ == 0))
    }
  }

  test("byteSize") {
    val table = Table[BigInt, Int](("x", "expected"), (0, 0), (1, 1), (255, 1), (256, 2), (65535, 2), (65536, 3),
      (BigInt(2).pow(256) - 1, 32), (BigInt(2).pow(256), 0))
    forAll(table) { (x, expected) =>
      assert(DataWord(x).byteSize === expected)
    }

    forAll(getDataWordGen()) { x =>
      import math._
      val byteSize = 1 + floor(log(x.doubleValue) / log(256)).toInt
      //assert(x.byteSize === byteSize)
    }
  }
}
