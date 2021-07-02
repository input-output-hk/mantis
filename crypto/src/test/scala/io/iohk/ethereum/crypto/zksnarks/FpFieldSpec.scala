package io.iohk.ethereum.crypto.zksnarks

import java.math.BigInteger

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.crypto.zksnark.FiniteField.Ops._
import io.iohk.ethereum.crypto.zksnark._

abstract class FieldSpec[T: FiniteField] extends AnyFunSuite with ScalaCheckPropertyChecks {
  val bigIntGen: Gen[BigInteger] = for {
    bytes <- Gen.listOfN(32, arbitrary[Byte])
  } yield new BigInteger(1, bytes.toArray)

  def fpGenerator: Gen[Fp] = bigIntGen.map(Fp(_)).retryUntil(fp => fp.isValid())

  def fp2Generator: Gen[Fp2] = for {
    fp1 <- fpGenerator
    fp2 <- fpGenerator
  } yield Fp2(fp1, fp1)

  def fp6Generator: Gen[Fp6] = for {
    fp1 <- fp2Generator
    fp2 <- fp2Generator
    fp3 <- fp2Generator
  } yield Fp6(fp1, fp2, fp3)

  def fp12Generator: Gen[Fp12] = for {
    fp1 <- fp6Generator
    fp2 <- fp6Generator
  } yield Fp12(fp1, fp2)

  //Generator of valid field elements, for which all laws needs to be obeyed
  def fpGen: Gen[T]

  implicit val config: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 100)

  test("a * b") {

    forAll(fpGen, fpGen) { (a: T, b: T) =>
      assert(a * b == b * a)
      assert((a * b).isValid())
    }
  }

  test("a + b") {
    forAll(fpGen, fpGen) { (a: T, b: T) =>
      assert(a + b == b + a)
      assert((a + b).isValid())
    }
  }

  test("a * a^-1 == one") {
    forAll(fpGen) { a: T =>
      assert(a * a.inversed() == FiniteField[T].one)
    }
  }

  test("a + (-a) == a - a == zero") {
    forAll(fpGen) { a: T =>
      assert(a + a.negated() == FiniteField[T].zero)
      assert(a - a == FiniteField[T].zero)
    }
  }

  test("a + (b + c) == (a + b) + c") {
    forAll(fpGen, fpGen, fpGen) { (a: T, b: T, c: T) =>
      assert(a + (b + c) == (a + b) + c)
    }
  }

  test("a * (b + c) == (a * b) + (a * c)") {
    forAll(fpGen, fpGen, fpGen) { (a: T, b: T, c: T) =>
      assert(a * (b + c) == a * b + a * c)
    }
  }

  test("0 as neutral element fo addition") {
    forAll(fpGen) { n1: T =>
      assert(n1 + FiniteField[T].zero == n1)
      assert(FiniteField[T].zero + n1 == n1)
    }
  }

  test("1 as neutral element fo multiplication") {
    forAll(fpGen) { n1: T =>
      assert(n1 * FiniteField[T].one == n1)
      assert(FiniteField[T].one * n1 == n1)
    }
  }

  test("multiply by 0") {
    forAll(fpGen) { n1: T =>
      assert(n1 * FiniteField[T].zero == FiniteField[T].zero)
      assert(FiniteField[T].zero * n1 == FiniteField[T].zero)
      assert((n1 * FiniteField[T].zero).isZero())
    }
  }

  test("-(a * b) == (-a) * b == a * (-b)") {
    forAll(fpGen, fpGen) { (a: T, b: T) =>
      assert((a * b).negated() == (a.negated()) * b)
      assert((a * b).negated() == a * (b.negated()))
    }
  }

  test("a.doubled == a + a") {
    forAll(fpGen) { a: T =>
      assert(a.doubled() == a + a)
    }
  }

  test("a.squared == a * a") {
    forAll(fpGen) { a: T =>
      assert(a.squared() == a * a)
    }
  }
}

class FpFieldSpec extends FieldSpec[Fp] {
  override def fpGen: Gen[Fp] = fpGenerator
}

class Fp2FieldSpec extends FieldSpec[Fp2] {
  override def fpGen: Gen[Fp2] = fp2Generator
}

class Fp6FieldSpec extends FieldSpec[Fp6] {
  override def fpGen: Gen[Fp6] = fp6Generator
}

class Fp12FieldSpec extends FieldSpec[Fp12] {
  override def fpGen: Gen[Fp12] = fp12Generator

  test("cyclotomic exp on fp12") {

    val input = Fp12(
      Fp6(
        Fp2(
          Fp(BigInt("2259924035228092997691937637688451143058635253053054071159756458902878894295")),
          Fp(BigInt("13145690032701362144460254305183927872683620413225364127064863863535255135244"))
        ),
        Fp2(
          Fp(BigInt("9910063591662383599552477067956819406417086889312288278252482503717089428441")),
          Fp(BigInt("537414042055419261990282459138081732565514913399498746664966841152381183961"))
        ),
        Fp2(
          Fp(BigInt("15311812409497308894370893420777496684951030254049554818293571309705780605004")),
          Fp(BigInt("13657107176064455789881282546557276003626320193974643644160350907227082365810"))
        )
      ),
      Fp6(
        Fp2(
          Fp(BigInt("4913017949003742946864670837361832856526234260447029873580022776602534856819")),
          Fp(BigInt("7834351480852267338070670220119081676575418514182895774094743209915633114041"))
        ),
        Fp2(
          Fp(BigInt("12837298223308203788092748646758194441270207338661891973231184407371206766993")),
          Fp(BigInt("12756474445699147370503225379431475413909971718057034061593007812727141391799"))
        ),
        Fp2(
          Fp(BigInt("9473802207170192255373153510655867502408045964296373712891954747252332944018")),
          Fp(BigInt("4583089109360519374075173304035813179013579459429335467869926761027310749713"))
        )
      )
    )

    val expected = Fp12(
      Fp6(
        Fp2(
          Fp(BigInt("14722956046055152398903846391223329501345567382234608299399030576415080188350")),
          Fp(BigInt("14280703280777926697010730619606819467080027543707671882210769811674790473417"))
        ),
        Fp2(
          Fp(BigInt("19969875076083990244184003223190771301761436396530543002586073549972410735411")),
          Fp(BigInt("10717335566913889643303549252432531178405520196706173198634734518494041323243"))
        ),
        Fp2(
          Fp(BigInt("6063612626166484870786832843320782567259894784043383626084549455432890717937")),
          Fp(BigInt("17089783040131779205038789608891431427943860868115199598200376195935079808729"))
        )
      ),
      Fp6(
        Fp2(
          Fp(BigInt("10029863438921507421569931792104023129735006154272482043027653425575205672906")),
          Fp(BigInt("6406252222753462799887280578845937185621081001436094637606245493619821542775"))
        ),
        Fp2(
          Fp(BigInt("1048245462913506652602966692378792381004227332967846949234978073448561848050")),
          Fp(BigInt("1444281375189053827455518242624554285012408033699861764136810522738182087554"))
        ),
        Fp2(
          Fp(BigInt("8839610992666735109106629514135300820412539620261852250193684883379364789120")),
          Fp(BigInt("11347360242067273846784836674906058940820632082713814508736182487171407730718"))
        )
      )
    )

    val result = Fp12.negExp(input, Fp12.pairingFinalExp)

    assert(result == expected)
  }
}
