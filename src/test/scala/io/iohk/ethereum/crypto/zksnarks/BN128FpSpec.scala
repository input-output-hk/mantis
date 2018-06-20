package io.iohk.ethereum.crypto.zksnarks

import io.iohk.ethereum.crypto.zksnark.BN128.Point
import io.iohk.ethereum.crypto.zksnark.{BN128Fp, Fp}
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class BN128FpSpec extends FunSuite with PropertyChecks {

  val curve = new BN128Fp

  val testData = Table[Fp, Fp, Fp](("x", "y", "z"),
    (Fp(1), Fp(2), Fp(1)),
    ( Fp(BigInt("1368015179489954701390400359078579693043519447331113978918064868415326638035")),
      Fp(BigInt("9918110051302171585080402603319702774565515993150576347155970296011118125764")),
      Fp(1))
  )

  test("P + P = 2 * P") {
    forAll(testData) {(x: Fp, y: Fp, z: Fp) =>
      val point = Point(x ,y, z)
      assert(curve.isOnCurve(point) && point.isValid)
      assert(curve.add(point, point) == curve.mul(point, 2))
    }
  }

  test("P + P + P = 3 * P") {
    forAll(testData) {(x: Fp, y: Fp, z: Fp) =>
      val point = Point(x ,y, z)
      val addingResult = curve.add(curve.add(point, point), point)
      val multiplyResult = curve.mul(point, 3)

      assert(curve.isOnCurve(point) && point.isValid)
      assert(curve.isOnCurve(addingResult) && curve.isOnCurve(multiplyResult))
      assert(addingResult == multiplyResult)

      val affinePoint = curve.toEthNotation(addingResult)
      assert(curve.isOnCurve(affinePoint))
    }
  }
}
