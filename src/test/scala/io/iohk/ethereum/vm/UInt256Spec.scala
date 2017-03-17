package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.vm.Generators._
import io.iohk.ethereum.vm.Int256Like._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.annotation.tailrec

class UInt256Spec extends FunSuite with PropertyChecks {

  val Modulus: BigInt = BigInt(2).pow(Size * 8)

  val Zero: UInt256 = UInt256(BigInt(0))

  val specialNumbers = Array(BigInt(-1), BigInt(0), BigInt(1), Modulus - 1, 1 - Modulus, 2 - Modulus)

  val pairs: Array[(BigInt, BigInt)] = specialNumbers
    .combinations(2)
    .map { case Array(n1, n2) => n1 -> n2 }
    .toArray

  val specialCases = Table(("n1", "n2"), pairs: _*)

  test("&") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) & UInt256(n2)) == UInt256(n1 & n2))
    }
    forAll(specialCases) { (n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) & UInt256(n2)) == UInt256(n1 & n2))
    }
  }

  test("|") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) | UInt256(n2)) == UInt256(n1 | n2))
    }
    forAll(specialCases) { (n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) | UInt256(n2)) == UInt256(n1 | n2))
    }
  }

  test("^") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      assert((UInt256(n1) ^ UInt256(n2)) == UInt256(n1 ^ n2))
    }
    forAll(specialCases) { (n1: BigInt, n2: BigInt) =>
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
    forAll(bigIntGen) { (n: BigInt) =>
      assert(-UInt256(n) == UInt256(-n))
    }
    forAll(Table(("n"), specialNumbers: _*)) { (n: BigInt) =>
      assert(-UInt256(n) == UInt256(-n))
    }
    assert(-UInt256(BigInt(1)) == UInt256(BigInt(-1)))
  }

  test("+") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) + UInt256(n2) == UInt256(n1 + n2))
    }
    forAll(specialCases) { (n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) + UInt256(n2) == UInt256(n1 + n2))
    }
  }

  test("-") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) - UInt256(n2) == UInt256(n1 - n2))
    }
    forAll(specialCases) { (n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) - UInt256(n2) == UInt256(n1 - n2))
    }
  }

  test("*") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
    forAll(specialCases) { (n1: BigInt, n2: BigInt) =>
      assert(UInt256(n1) * UInt256(n2) == UInt256(n1 * n2))
    }
  }

  test("div") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((UInt256(n1) div UInt256(n2)) == UInt256(n1 / n2))
      }
    }
    val divSpecialCases = Table[BigInt, BigInt, UInt256](
      ("n1", "n2", "result"),
      (-1, 1, UInt256(Modulus - 1)),
      (-1, Modulus - 1, UInt256(1)),
      (-1, 1 - Modulus, UInt256(Modulus - 1)),
      (0, 1, Zero),
      (0, Modulus - 1, Zero),
      (0, 1 - Modulus, Zero),
      (0, 2 - Modulus, Zero),
      (1, Modulus - 1, Zero),
      (1, 1 - Modulus, UInt256(1)),
      (1, 2 - Modulus, Zero),
      (Modulus - 1, 1 - Modulus, UInt256(Modulus - 1)),
      (1 - Modulus, 2 - Modulus, Zero))
    forAll(divSpecialCases) { (n1, n2, result) =>
      whenever(n2 != 0) {
        assert((UInt256(n1) div UInt256(n2)) == result)
      }
    }
    assertThrows[ArithmeticException](UInt256(1) div Zero)
  }

  test("%") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        assert((UInt256(n1) % UInt256(n2)) == UInt256(n1 % n2))
      }
    }
    assert((UInt256(-1) % UInt256(Modulus - 1)) == Zero)
    assertThrows[ArithmeticException](UInt256(1) % Zero)
  }

  test("addmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(UInt256(n1).addmod(UInt256(n2), UInt256(n3)) == UInt256((n1 + n2) mod n3))
      }
    }
    assertThrows[ArithmeticException](UInt256(42).addmod(UInt256(42), Zero))
  }

  test("mulmod") {
    forAll(bigIntGen, bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt, n3: BigInt) =>
      whenever(n3 != 0) {
        assert(UInt256(n1).mulmod(UInt256(n2), UInt256(n3)) == UInt256((n1 * n2) mod n3))
      }
    }
    assertThrows[ArithmeticException](UInt256(42).mulmod(UInt256(42), Zero))
  }

  @tailrec
  private def isRelativelyPrime(a: BigInt, b: BigInt): Boolean =
    if (b == 0) a.abs == 1 else isRelativelyPrime(b, a % b)

  test("**") {
    forAll(bigIntGen, bigIntGen) { (n1: BigInt, n2: BigInt) =>
      whenever(n2 != 0) {
        if (!isRelativelyPrime(n1, Modulus) && n2 < 0)
          assertThrows[ArithmeticException](UInt256(n1) ** UInt256(n2))
        else
          assert(UInt256(n1) ** UInt256(n2) == UInt256(n1.modPow(n2, Modulus)))
      }
    }
    assert(UInt256(3) ** UInt256(7) == UInt256(2187))
  }

  test("getByte") {
    val dw1 = UInt256(ByteString((100 to 131).map(_.toByte).toArray))

    val testData = Table[UInt256, UInt256, UInt256](
      ("word", "idx", "result"),
      (UInt256(42), UInt256(-1), Zero),
      (UInt256(42), UInt256(Size), Zero),
      (UInt256(42), UInt256(Size + 1), Zero),
      (UInt256(1), UInt256(287), Zero),
      (dw1, UInt256(0), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 100.toByte)),
      (dw1, UInt256(1), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ 101.toByte)),
      (dw1, UInt256(30), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -126.toByte)),
      (dw1, UInt256(31), UInt256(ByteString(Array.fill[Byte](Size - 1)(0)) :+ -125.toByte)),
      (UInt256(ByteString(Array.fill[Byte](32)(-50))), UInt256(8), UInt256(ByteString(-50))))

    forAll(testData) { (a, b, result) =>
      assert(a.getByte(b) == result)
    }
  }

  test("numeric values") {
    forAll(byteGen, shortGen, intGen, longGen) { (b: Byte, s: Short, i: Int, l: Long) =>
      UInt256(b).byteValue == b
      UInt256(s).shortValue == s
      UInt256(i).intValue == i
      UInt256(l).longValue == l
      UInt256(l).floatValue == l.toFloat
      UInt256(l).doubleValue == l.toDouble
    }
    assert(specialNumbers.map(UInt256(_).byteValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(UInt256(_).shortValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(UInt256(_).intValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(UInt256(_).longValue).toSeq == Seq(-1, 0, 1, -1, 1, 2))
    assert(specialNumbers.map(UInt256(_).floatValue).toSeq == Seq(Float.PositiveInfinity, 0.0, 1.0, Float.PositiveInfinity, 1.0, 2.0))
    assert(specialNumbers.map(UInt256(_).doubleValue).toSeq == Seq(1.157920892373162E77, 0.0, 1.0, 1.157920892373162E77, 1.0, 2.0))
  }

  test("comparison") {
    type CFUINT = (UInt256, UInt256) => Boolean
    type CFBI = (BigInt, BigInt) => Boolean
    case class Cmp(uint: CFUINT, bi: CFBI)

    val cmpFuncUInt256 = Seq[CFUINT](_ > _, _ >= _, _ < _, _ <= _)
    val cmpFuncBigInt = Seq[CFBI](_ > _, _ >= _, _ < _, _ <= _)
    val comparators = cmpFuncUInt256.zip(cmpFuncBigInt).map(Cmp.tupled)

    val uin256Gen = bigIntGen.map(UInt256(_))

    forAll(Table("comparators", comparators: _*)) { cmp =>
      forAll(bigIntGen, bigIntGen) { (a, b) =>
        val (x, y) = (UInt256(a), UInt256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
      }
      forAll(specialCases) { (a, b) =>
        val (x, y) = (UInt256(a), UInt256(b))
        assert(cmp.uint(x, y) == cmp.bi(x.n, y.n))
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

  test("conversion between signed and unsigned int") {
    forAll(getUInt256Gen()) { x: UInt256 =>
      assert(x === UInt256(Int256(x)))
    }
  }

}

