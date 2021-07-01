package io.iohk.ethereum.crypto.zksnarks

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableFor3
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.crypto.zksnark.BN128.Point
import io.iohk.ethereum.crypto.zksnark.BN128Fp
import io.iohk.ethereum.crypto.zksnark.Fp

class BN128FpSpec extends AnyFunSuite with ScalaCheckPropertyChecks {

  val testData: TableFor3[Fp, Fp, Fp] = Table[Fp, Fp, Fp](
    ("x", "y", "z"),
    (Fp(1), Fp(2), Fp(1)),
    (
      Fp(BigInt("1368015179489954701390400359078579693043519447331113978918064868415326638035")),
      Fp(BigInt("9918110051302171585080402603319702774565515993150576347155970296011118125764")),
      Fp(1)
    ),
    (
      Fp(BigInt("11169198337205317385038692134282557493133418158128574038999810944352461077961")),
      Fp(BigInt("2820885980102468247213289930888494165190958823101043243711917453290081841766")),
      Fp(1)
    ),
    (
      Fp(BigInt("3510227910005969626168871163796842095937160976810256674232777209574668193517")),
      Fp(BigInt("2885800476071299445182650755020278501280179672256593791439003311512581969879")),
      Fp(1)
    ),
    (
      Fp(BigInt("15497584038690294240042153688304417339506091937513459124271972833238779664131")),
      Fp(BigInt("21762456842531143558012592863461237297422391564814111359902381816272400009493")),
      Fp(1)
    )
  )

  test("P + P = 2 * P") {
    forAll(testData) { (x: Fp, y: Fp, z: Fp) =>
      val point = Point(x, y, z)
      assert(BN128Fp.isOnCurve(point) && point.isValid)
      assert(BN128Fp.add(point, point) == BN128Fp.mul(point, 2))
    }
  }

  test("P + P + P = 3 * P") {
    forAll(testData) { (x: Fp, y: Fp, z: Fp) =>
      val point = Point(x, y, z)
      val addingResult = BN128Fp.add(BN128Fp.add(point, point), point)
      val multiplyResult = BN128Fp.mul(point, 3)

      assert(BN128Fp.isOnCurve(point) && point.isValid)
      assert(BN128Fp.isOnCurve(addingResult) && BN128Fp.isOnCurve(multiplyResult))
      assert(addingResult == multiplyResult)

      val affinePoint = BN128Fp.toEthNotation(addingResult)
      assert(BN128Fp.isOnCurve(affinePoint))
    }
  }
}
