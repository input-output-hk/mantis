package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import io.iohk.ethereum.vm.UInt256._

class UInt256Spec extends FunSuite with PropertyChecks {

  val Modulus: BigInt = UInt256.MaxValue.toBigInt + 1
  val MaxSignedValue: BigInt = Modulus / 2 - 1

  val specialNumbers = Seq(BigInt(-1), BigInt(0), BigInt(1), MaxValue.toBigInt, -MaxValue.toBigInt, -MaxValue.toBigInt + 1)

  val pairs: Seq[(BigInt, BigInt)] = specialNumbers
    .combinations(2)
    .map {case Seq(n1, n2) => n1 -> n2}
    .toSeq

  val specialCases = Table(("n1", "n2"), pairs: _*)

  def toSignedBigInt(n: BigInt): BigInt = if (n > MaxSignedValue) n - Modulus else n

  def toUnsignedBigInt(n: BigInt): BigInt = if (n < 0) n + Modulus else n

  /** For each operation (op) tests check a following property:
   For two BigInts (n1, n2):
   UInt256(n1) op UInt256(n2) == UInt256(n1 op n2)
   */
  test("&") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) & UInt256(n2)) == UInt256(n1 & n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) & UInt256(n2)) == UInt256(n1 & n2))
    }
  }

  test("|") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) | UInt256(n2)) == UInt256(n1 | n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) | UInt256(n2)) == UInt256(n1 | n2))
    }
  }

  test("^") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) ^ UInt256(n2)) == UInt256(n1 ^ n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) ^ UInt256(n2)) == UInt256(n1 ^ n2))
    }
  }

  test("~") {
    forAll(bigIntGen) { n: BigInt =>
      assert(~UInt256(n) == UInt256(~n))
    }
    forAll(Table("n", specialNumbers: _*)) { n: BigInt =>
      assert(~UInt256(n) == UInt256(~n))
    }
  }

  test("negation") {
    forAll(bigIntGen) {(n: BigInt) =>
      assert(-UInt256(n) == UInt256(-n))
    }
    forAll(Table("n", specialNumbers: _*)) {(n: BigInt) =>
      assert(-UInt256(n) == UInt256(-n))
    }
    assert(-UInt256(1) == UInt256(-1))
    assert(-UInt256(-1) == UInt256(1))
    assert(-UInt256.Zero == UInt256.Zero)
  }

  test("+") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) + UInt256(n2) == UInt256(n1 + n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) + UInt256(n2) == UInt256(n1 + n2))
    }
  }

  test("-") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) - UInt256(n2) == UInt256(n1 - n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) - UInt256(n2) == UInt256(n1 - n2))
    }
  }

  test("*") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
  }

  test("/") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
  }

  test("div") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((UInt256(n1) div UInt256(n2)) == UInt256(n1 / n2))
      }
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      whenever(n1 > 0 && n2 > 0) {
        assert((UInt256(n1) div UInt256(n2)) == UInt256(n1 / n2))
      }
    }
    assert((UInt256(1) div Zero) == Zero)
  }

  test("sdiv") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        val expected: BigInt = toUnsignedBigInt(toSignedBigInt(n1) / toSignedBigInt(n2))
        assert((UInt256(n1) sdiv UInt256(n2)) == UInt256(expected))
      }
    }
    assert((UInt256(-1) sdiv UInt256(-MaxValue.toBigInt)) == UInt256(-1))
    assert((UInt256(-1) sdiv Zero) == Zero)
  }

  test("mod") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((UInt256(n1) mod UInt256(n2)) == UInt256(n1 mod n2))
      }
    }
    assert((UInt256(-1) mod UInt256(MaxValue.toBigInt)) == Zero)
    assert((UInt256(1) mod Zero) == Zero)
  }

  test("smod") {
    assert((UInt256(Modulus - 1) smod UInt256(3)) == UInt256(Modulus - 1))
    assert((UInt256(-1) smod UInt256(MaxValue.toBigInt)) == Zero)
    assert((UInt256(1) smod Zero) == Zero)
  }

  test("addmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(UInt256(n1).addmod(UInt256(n2), UInt256(n3)) == UInt256((n1 + n2) mod n3))
      }
    }
    assert(UInt256(42).addmod(UInt256(42), Zero) == Zero)
  }

  test("mulmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(UInt256(n1).mulmod(UInt256(n2), UInt256(n3)) == UInt256((n1 * n2) mod n3))
      }
    }
    assert(UInt256(42).mulmod(UInt256(42), Zero) == Zero)
  }

  test("**") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) ** UInt256(n2) == UInt256(n1.modPow(n2, Modulus)))
    }
  }

  test("signExtend") {
    val testData = Table[UInt256, UInt256, UInt256](
      ("value", "extension", "result"),
      (42, 3, 42),
      (42, 1, 42),
      (42, -1, 42),
      (42, 0, 42),
      (42, Size, 42),
      (42, Size + 1, 42),
      (-42, Size, -42),
      (-42, Size + 1, -42),
      (-42, -11, -42),
      (-1, 1, -1),
      (-1, 1, -1),
      (0x1a81ff, 1, UInt256(Array.fill[Byte](30)(-1) ++ Array(0x81, 0xff).map(_.toByte))),
      (0x1a81ff, 2, 0x1a81ff),
      (0x1a81ff, 10, 0x1a81ff)
    )

    forAll(testData) { (uint, extension, result) =>
      assert(uint.signExtend(extension) == result)
    }
  }

  test("slt") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) slt UInt256(n2)) == (toSignedBigInt(n1) < toSignedBigInt(n2)))
    }

    val testData = Table[UInt256, UInt256, Boolean](
      ("a", "b", "result"),
      (-1, 1, true),
      (1, -1, false),
      (1, 0, false))

    forAll(testData) { (a, b, result) =>
      assert((a slt b) == result)
    }
  }

  test("sgt") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) sgt UInt256(n2)) == (toSignedBigInt(n1) > toSignedBigInt(n2)))
    }

    val testData = Table[UInt256, UInt256, Boolean](
      ("a", "b", "result"),
      (-1, 1, false),
      (1, -1, true),
      (0, 1, false),
      (1, 0, true))

    forAll(testData) { (a, b, result) =>
      assert((a sgt b) == result)
    }
  }

  test("getByte") {
    val v1 = UInt256(ByteString((100 to 131).map(_.toByte).toArray))

    val testData = Table[UInt256, UInt256, UInt256](
      ("value", "idx", "result"),
      (42, -1, Zero),
      (42, Size, Zero),
      (42, Size + 1, Zero),
      (1, 287, Zero),
      (v1, 0, 100),
      (v1, 1, 101),
      (v1, 30, 130),
      (v1, 31, 131),
      (UInt256(Array.fill[Byte](32)(-50)), 8, 256 - 50))

    forAll(testData) { (a, b, result) =>
      assert(a.getByte(b) == result)
    }
  }

  test("intValue") {
    assert(specialNumbers.map(UInt256(_).toInt) == Seq(Int.MaxValue, 0, 1, Int.MaxValue, 1, 2))
  }

  test("comparison") {
    type CFUI = (UInt256, UInt256) => Boolean
    type CFBI = (BigInt, BigInt) => Boolean
    case class Cmp(uint: CFUI, bi: CFBI)

    val cmpFuncUInt256 = Seq[CFUI](_ > _, _ >= _, _ < _, _ <= _)
    val cmpFuncBigInt   = Seq[CFBI](_ > _, _ >= _, _ < _, _ <= _)
    val comparators = cmpFuncUInt256.zip(cmpFuncBigInt).map(Cmp.tupled)

    val uint256Gen = getUInt256Gen()

    forAll(Table("comparators", comparators: _*)) { cmp =>
      forAll(uint256Gen, uint256Gen) { (a, b) =>
        assert(cmp.uint(a, b) == cmp.bi(a.toBigInt, b.toBigInt))
      }

      forAll(specialCases) { (x, y) =>
        val (a, b) = (UInt256(x), UInt256(y))
        assert(cmp.uint(a, b) == cmp.bi(a.toBigInt, b.toBigInt))
      }
    }
  }

  test("Passing too long ByteString should throw an exception") {
    assertThrows[IllegalArgumentException] {
      UInt256(ByteString(Array.fill(Size + 1)(1.toByte)))
    }
  }

  test("UInt256 converted to a byte array should always have length 32 bytes") {
    forAll(bigIntGen) { n =>
      assert(UInt256(n).bytes.size == 32)
    }
    // regression
    assert(UInt256(BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935")).bytes.size == 32)
  }

  test("2-way bytes conversion") {
    forAll(getUInt256Gen()) { x =>
      val y = UInt256(x.bytes)
      assert(x === y)
    }

    forAll(getByteStringGen(0, 32)) { xs =>
      val ys = UInt256(xs).bytes
      assert(xs.dropWhile(_ == 0) === ys.dropWhile(_ == 0))
    }
  }

  test("byteSize") {
    val table = Table[BigInt, Int](("x", "expected"), (0, 0), (1, 1), (255, 1), (256, 2), (65535, 2), (65536, 3),
      (BigInt(2).pow(256) - 1, 32), (BigInt(2).pow(256), 0))
    forAll(table) { (x, expected) =>
      assert(UInt256(x).byteSize === expected)
    }

    forAll(getUInt256Gen(min = UInt256(1))) { x =>
      import math._
      val byteSize = 1 + floor(log(x.doubleValue) / log(256)).toInt
      assert(x.byteSize === byteSize)
    }
  }
}
