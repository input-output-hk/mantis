package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import io.iohk.ethereum.vm.Int256Like._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.annotation.tailrec


class Int256Spec extends FunSuite with PropertyChecks {

  val Modulus: BigInt = BigInt(2).pow(Size * 8)

  val Zero: Int256 = Int256(BigInt(0))

  val MaxSignedValue: BigInt = BigInt(2).pow(Size * 8 - 1) - 1

  val specialNumbers = Array(BigInt(-1), BigInt(0), BigInt(1), Modulus - 1, 1 - Modulus, 2 - Modulus)

  val pairs: Array[(BigInt, BigInt)] = specialNumbers
    .combinations(2)
    .map {case Array(n1, n2) => n1 -> n2}
    .toArray

  val specialCases = Table(("n1", "n2"), pairs: _*)

  def toSignedBigInt(n: BigInt): BigInt = {
    val m = (n % Modulus + Modulus) % Modulus
    if (m > MaxSignedValue) m - Modulus else m
  }

  test("&") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) & Int256(n2)) == Int256(n1 & n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) & Int256(n2)) == Int256(n1 & n2))
    }
  }

  test("|") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) | Int256(n2)) == Int256(n1 | n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) | Int256(n2)) == Int256(n1 | n2))
    }
  }

  test("^") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) ^ Int256(n2)) == Int256(n1 ^ n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert((Int256(n1) ^ Int256(n2)) == Int256(n1 ^ n2))
    }
  }

  test("~") {
    forAll(bigIntGen) { n: BigInt =>
      assert(~Int256(n) == Int256(~n))
    }
    forAll(Table("n", specialNumbers: _*)) { n: BigInt =>
      assert(~Int256(n) == Int256(~n))
    }
  }

  test("negation") {
    forAll(bigIntGen) {(n: BigInt) =>
      assert(-Int256(n) == Int256(-n))
    }
    forAll(Table(("n"), specialNumbers: _*)) {(n: BigInt) =>
      assert(-Int256(n) == Int256(-n))
    }
    assert(-Int256(BigInt(1)) == Int256(BigInt(-1)))
  }

  test("+") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) + Int256(n2) == Int256(n1 + n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) + Int256(n2) == Int256(n1 + n2))
    }
  }

  test("-") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) - Int256(n2) == Int256(n1 - n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) - Int256(n2) == Int256(n1 - n2))
    }
  }

  test("*") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) * Int256(n2) == Int256(n1 * n2))
    }
    forAll(specialCases) {(n1: BigInt, n2: BigInt) =>
      assert(Int256(n1) * Int256(n2) == Int256(n1 * n2))
    }
  }

  test("div") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((Int256(n1) div Int256(n2)) == Int256(toSignedBigInt(n1) / toSignedBigInt(n2)))
      }
    }
    val divSpecialCases = Table[BigInt, BigInt, Int256](
      ("n1", "n2", "result"),
      (-1, 1, Int256(Modulus - 1)),
      (-1, Modulus - 1, Int256(1)),
      (-1, 1 - Modulus, Int256(Modulus - 1)),
      (0, 1, Zero),
      (0, Modulus - 1, Zero),
      (0, 1 - Modulus, Zero),
      (0, 2 - Modulus, Zero),
      (1, Modulus - 1, Int256(-1)),
      (1, 1 - Modulus, Int256(1)),
      (1, 2 - Modulus, Zero),
      (Modulus - 1, 1 - Modulus, Int256(Modulus - 1)),
      (1 - Modulus, 2 - Modulus, Zero))
    forAll(divSpecialCases) { (n1, n2, result) =>
      whenever(n2 != 0) {
        assert((Int256(n1) div Int256(n2)) == result)
      }
    }
    assertThrows[ArithmeticException](Int256(1) div Zero)
  }

  test("%") {
    forAll(bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((Int256(n1) % Int256(n2)) == Int256(toSignedBigInt(n1) % toSignedBigInt(n2)))
      }
    }
    assert((Int256(-1) % Int256(Modulus - 1)) == Zero)
    assertThrows[ArithmeticException]((Int256(1) % Zero))
  }

  test("addmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(Int256(n1).addmod(Int256(n2), Int256(n3)) == Int256((toSignedBigInt(n1) + toSignedBigInt(n2)) % toSignedBigInt(n3)))
      }
    }
    assertThrows[ArithmeticException](Int256(42).addmod(Int256(42), Zero))
  }

  test("mulmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) {(n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(Int256(n1).mulmod(Int256(n2), Int256(n3)) == Int256((toSignedBigInt(n1) * toSignedBigInt(n2)) % toSignedBigInt(n3)))
      }
    }
    assertThrows[ArithmeticException](Int256(42).mulmod(Int256(42), Zero))
  }

  @tailrec
  private def isRelativelyPrime(a: BigInt, b: BigInt): Boolean =
    if (b == 0) a.abs == 1 else isRelativelyPrime(b, a % b)

  test("**") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        val sn1 = toSignedBigInt(n1)
        val sn2 = toSignedBigInt(n2)
        if (!isRelativelyPrime(sn1, Modulus) && sn2 < 0)
          assertThrows[ArithmeticException](Int256(n1) ** Int256(n2))
        else
          assert(Int256(n1) ** Int256(n2) == Int256(sn1.modPow(sn2, Modulus)))
      }
    }
    assert(Int256(3) ** Int256(7) == Int256(2187))
    assertThrows[ArithmeticException](Int256(-2) ** Int256(-1))
  }

  test("signExtend") {
    val testData = Table[Int256, Int256, Int256](
      ("word", "extension", "result"),
      (Int256(42), Int256(3), Int256(42)),
      (Int256(42), Int256(1), Int256(42)),
      (Int256(42), Int256(-1), Int256(42)),
      (Int256(42), Int256(0), Int256(42)),
      (Int256(42), Int256(Size), Int256(42)),
      (Int256(42), Int256(Size + 1), Int256(42)),
      (Int256(-42), Int256(Size), Int256(-42)),
      (Int256(-42), Int256(Size + 1), Int256(-42)),
      (Int256(-42), Int256(-11), Int256(-42)),
      (Int256(-1), Int256(1), Int256(-0xff01)),
      (Int256(0x1a81ff), Int256(1), Int256(Array.fill[Byte](30)(-1) ++ Array[Byte](-127, -1))))

    forAll(testData) { (word, extension, result) =>
      assert((word).signExtend(extension) == result)
    }
  }

  test("getByte") {
    val dw1 = Int256(ByteString((100 to 131).map(_.toByte).toArray))

    val testData = Table[Int256, Int256, Int256](
      ("word", "idx", "result"),
      (Int256(42), Int256(-1), Zero),
      (Int256(42), Int256(Size), Zero),
      (Int256(42), Int256(Size + 1), Zero),
      (Int256(1), Int256(287), Zero),
      (dw1, Int256(0), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 100.toByte)),
      (dw1, Int256(1), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 101.toByte)),
      (dw1, Int256(30), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -126.toByte)),
      (dw1, Int256(31), Int256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -125.toByte)),
      (Int256(ByteString(Array.fill[Byte](32)(-50))), Int256(8), Int256(ByteString(-50))))

    forAll(testData) { (a, b, result) =>
      assert(a.getByte(b) == result)
    }
  }

  test("numeric values") {
    forAll(byteGen, shortGen, intGen, longGen) { (b: Byte, s: Short, i: Int, l: Long) =>
        Int256(b).byteValue == b
        Int256(s).shortValue == s
        Int256(i).intValue == i
        Int256(l).longValue == l
        Int256(l).floatValue == l.toFloat
        Int256(l).doubleValue == l.toDouble
    }
    assert(specialNumbers.map(Int256(_).byteValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(Int256(_).shortValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(Int256(_).intValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(Int256(_).longValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(Int256(_).floatValue).toSeq == Seq(-1.0, 0.0, 1.0, -1.0, 1.0, 2.0))
    assert(specialNumbers.map(Int256(_).doubleValue).toSeq == Seq(-1.0, 0.0, 1.0, -1.0, 1.0, 2.0))
  }

  test("comparison") {
    type CFINT = (Int256, Int256) => Boolean
    type CFBI = (BigInt, BigInt) => Boolean
    case class Cmp(uint: CFINT, bi: CFBI)

    val cmpFuncInt256 = Seq[CFINT](_ > _, _ >= _, _ < _, _ <= _)
    val cmpFuncBigInt = Seq[CFBI](_ > _, _ >= _, _ < _, _ <= _)
    val comparators = cmpFuncInt256.zip(cmpFuncBigInt).map(Cmp.tupled)

    val int256Gen = bigIntGen.map(Int256(_))

    forAll(Table("comparators", comparators: _*)) { cmp =>
      forAll(bigIntGen, bigIntGen) { (a, b) =>
        val (x, y) = (Int256(a), Int256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
      }
      forAll(specialCases) { (a, b) =>
        val (x, y) = (Int256(a), Int256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
      }
    }
  }

  test("Passing too long ByteString should throw an exception") {
    assertThrows[IllegalArgumentException] {
      Int256(ByteString(Array.fill(Size + 1)(1.toByte)))
    }
  }

  test("Int256 converted to a byte array should always have length 32 bytes") {
    forAll(bigIntGen) { n =>
      assert(Int256(n).bytes.size == 32)
    }
    // regression
    assert(Int256(BigInt("115792089237316195423570985008687907853269984665640564039457584007913129639935")).bytes.size == 32)
  }

  test("2-way bytes conversion") {
    forAll(getInt256Gen()) { x =>
      val y = Int256(x.bytes)
      assert(x.bytes === y.bytes)
    }

    forAll(getByteStringGen(0, 32)) { xs =>
      val ys = Int256(xs).bytes
      assert(xs.dropWhile(_ == 0) === ys.dropWhile(_ == 0))
    }
  }

  test("conversion between signed and unsigned int") {
    forAll(getInt256Gen()) { x: Int256 =>
      assert(x === Int256(UInt256(x)))
    }
  }

}
