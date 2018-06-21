package io.iohk.ethereum.crypto.zksnarks

import io.iohk.ethereum.crypto.zksnark.{FiniteField, Fp}
import io.iohk.ethereum.crypto.zksnark.FiniteField.Ops._
import io.iohk.ethereum.vm.Generators._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class FpFieldSpec extends FunSuite with PropertyChecks {

  //Generator of valid field elements, for which all laws needs to be obeyed
  def fpGen: Gen[Fp] = bigIntGen.map(Fp(_)).retryUntil(fp => fp.isValid())

  test("a * b") {
    forAll(fpGen, fpGen) {(a: Fp, b: Fp) =>
      assert(a * b == b * a)
      assert((a * b).isValid())
    }
  }

  test("a + b") {
    forAll(fpGen, fpGen) {(a: Fp, b: Fp) =>
      assert(a + b == b + a)
      assert((a + b).isValid())
    }
  }

  test("a * a^-1 == one") {
    forAll(fpGen) {a: Fp =>
      assert(a * a.inversed() == FiniteField[Fp].one)
    }
  }

  test("a + (-a) == a - a == zero") {
    forAll(fpGen) {a: Fp =>
      assert(a + a.negated() == FiniteField[Fp].zero)
      assert(a - a == FiniteField[Fp].zero)
    }
  }

  test("a + (b + c) == (a + b) + c") {
    forAll(fpGen, fpGen, fpGen) {(a: Fp, b: Fp, c: Fp) =>
      assert(a + (b + c) == (a + b) + c)
    }
  }

  test("a * (b + c) == (a * b) + (a * c)") {
    forAll(fpGen, fpGen, fpGen) {(a: Fp, b: Fp, c: Fp) =>
      assert(a * (b + c) == a * b + a * c)
    }
  }

  test("0 as neutral element fo addition") {
    forAll(fpGen) {n1: Fp =>
      assert(n1 + FiniteField[Fp].zero == n1)
      assert(FiniteField[Fp].zero + n1 == n1)
    }
  }

  test("1 as neutral element fo multiplication") {
    forAll(fpGen) {n1: Fp =>
      assert(n1 * FiniteField[Fp].one == n1)
      assert(FiniteField[Fp].one * n1 == n1)
    }
  }

  test("multiply by 0") {
    forAll(fpGen) {n1: Fp =>
      assert(n1 * FiniteField[Fp].zero == FiniteField[Fp].zero)
      assert(FiniteField[Fp].zero * n1 == FiniteField[Fp].zero)
    }
  }

  test("-(a * b) == (-a) * b == a * (-b)") {
    forAll(fpGen, fpGen) {(a: Fp, b: Fp) =>
      assert((a * b).negated() == (a.negated()) * b)
      assert((a * b).negated() == a * (b.negated()))
    }
  }

  test("a.doubled == a + a") {
    forAll(fpGen) {a: Fp =>
      assert(a.doubled() == a + a)
    }
  }

  test("a.squared == a * a") {
    forAll(fpGen) {a: Fp =>
      assert(a.squared() == a * a)
    }
  }
}
