package io.iohk.ethereum.crypto.zksnark

import scala.collection.mutable.ArrayBuffer

import io.iohk.ethereum.crypto.zksnark.BN128.BN128G1
import io.iohk.ethereum.crypto.zksnark.BN128.BN128G2
import io.iohk.ethereum.crypto.zksnark.BN128.Point
import io.iohk.ethereum.crypto.zksnark.FiniteField.Ops._

object PairingCheck {

  val loopCount: BigInt = BigInt("29793968203157093288")

  /** Pairing function is defined as: `e: G_1 x G_2 -> G_T` where G1 is element of [[io.iohk.ethereum.crypto.zksnark.BN128.BN128G1]]
    * and G2 is element of [[io.iohk.ethereum.crypto.zksnark.BN128.BN128G2]]
    *
    * Description of algorithms in <a href="https://eprint.iacr.org/2010/354.pdf">optimal ate pairing</a>
    *
    * Arithmetic has been ported from <a href="https://github.com/scipr-lab/libff/blob/master/libff/algebra/curves/alt_bn128/alt_bn128_pairing.cpp">libff</a>
    * Ate pairing algorithms
    *
    * @param pairs Sequence of valid pairs of group elements (a1, b1, a2, b2, ..., ak, bk) from (G_1 x G_2)^k^
    * @return `true` if `log_P1(a1) * log_P2(b1) + ... + log_P1(ak) * log_P2(bk) = 0` else `false`
    */
  def pairingCheck(pairs: Seq[G1G2Pair]): Boolean = {
    val product = pairs.foldLeft(FiniteField[Fp12].one) { (acc, pair) =>
      val miller = millerLoop(pair.a, pair.b)
      if (miller != FiniteField[Fp12].one)
        acc * miller
      else
        acc
    }

    Fp12.finalExp(product) == FiniteField[Fp12].one
  }

  private def millerLoop(g1: BN128G1, g2: BN128G2): Fp12 =
    if (g1.p.isZero || g2.p.isZero) {
      FiniteField[Fp12].one
    } else {
      val g1Affine = BN128Fp.toAffineCoordinates(g1.p)
      val g2Affine = BN128Fp2.toAffineCoordinates(g2.p)

      val coeffs = calcEllCoeffs(g2Affine)

      var f = FiniteField[Fp12].one
      var idx = 0
      var i = loopCount.bitLength - 2 //every bit except most significant one

      while (i >= 0) {
        var c = coeffs(idx)
        idx += 1

        f = f.squared()
        f = Fp12.mulBy024(f, c.ell0, Fp2.mulByConst(c.ellW, g1Affine.y), Fp2.mulByConst(c.ellV, g1Affine.x))

        if (loopCount.testBit(i)) {
          c = coeffs(idx)
          idx += 1
          f = Fp12.mulBy024(f, c.ell0, Fp2.mulByConst(c.ellW, g1Affine.y), Fp2.mulByConst(c.ellV, g1Affine.x))
        }

        i = i - 1
      }

      var c = coeffs(idx)
      idx += 1
      f = Fp12.mulBy024(f, c.ell0, Fp2.mulByConst(c.ellW, g1Affine.y), Fp2.mulByConst(c.ellV, g1Affine.x))

      c = coeffs(idx)
      f = Fp12.mulBy024(f, c.ell0, Fp2.mulByConst(c.ellW, g1Affine.y), Fp2.mulByConst(c.ellV, g1Affine.x))

      f
    }

  private def calcEllCoeffs(base: Point[Fp2]): Seq[EllCoeffs] = {
    val coeffs = new ArrayBuffer[EllCoeffs]()

    var i = loopCount.bitLength - 2 //every bit except most significant one

    var addend = base

    while (i >= 0) {
      val doubling = flippedMillerLoopDoubling(addend)

      addend = doubling.g2

      coeffs += doubling.coeffs

      if (loopCount.testBit(i)) {
        val addition = flippedMillerLoopMixedAddition(base, addend)
        addend = addition.g2
        coeffs += addition.coeffs
      }

      i = i - 1
    }

    val q1 = BN128G2.mulByP(base)
    var q2 = BN128G2.mulByP(q1)

    q2 = Point[Fp2](q2.x, q2.y.negated(), q2.z)

    var addition = flippedMillerLoopMixedAddition(q1, addend)
    addend = addition.g2
    coeffs += addition.coeffs

    addition = flippedMillerLoopMixedAddition(q2, addend)
    coeffs += addition.coeffs

    coeffs.toVector
  }

  private def flippedMillerLoopMixedAddition(base: Point[Fp2], addend: Point[Fp2]): Precomputed = {
    val (x1, y1, z1) = (addend.x, addend.y, addend.z)
    val (x2, y2) = (base.x, base.y)

    val d = x1 - (x2 * z1)
    val e = y1 - (y2 * z1)
    val f = d.squared()
    val g = e.squared()
    val h = d * f
    val i = x1 * f
    val j = h + (z1 * g) - i.doubled()

    val x3 = d * j
    val y3 = e * (i - j) - (h * y1)
    val z3 = z1 * h

    val ell0 = Fp2.NON_RESIDUE * ((e * x2) - (d * y2))
    val ellV = e.negated()
    val ellW = d

    Precomputed(Point[Fp2](x3, y3, z3), EllCoeffs(ell0, ellW, ellV))
  }

  private def flippedMillerLoopDoubling(g2: Point[Fp2]): Precomputed = {
    val (x, y, z) = (g2.x, g2.y, g2.z)

    val a = Fp2.mulByConst(x * y, Fp.twoInv)
    val b = y.squared()
    val c = z.squared()
    val d = c + c + c
    val e = Fp2.B_Fp2 * d
    val f = e + e + e
    val g = Fp2.mulByConst(b + f, Fp.twoInv)
    val h = (y + z).squared() - (b + c)
    val i = e - b
    val j = x.squared()
    val e2 = e.squared()
    val rx = a * (b - f)
    val ry = g.squared() - (e2 + e2 + e2)
    val rz = b * h

    val ell0 = Fp2.NON_RESIDUE * i
    val ellW = h.negated()
    val ellV = j + j + j
    Precomputed(Point[Fp2](rx, ry, rz), EllCoeffs(ell0, ellW, ellV))
  }

  case class Precomputed(g2: Point[Fp2], coeffs: EllCoeffs)
  case class EllCoeffs(ell0: Fp2, ellW: Fp2, ellV: Fp2)
  case class G1G2Pair(a: BN128G1, b: BN128G2)
}
