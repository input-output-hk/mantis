package io.iohk.ethereum.crypto.zksnark

import akka.util.ByteString
import io.iohk.ethereum.crypto.zksnark.BN128.Point
import io.iohk.ethereum.crypto.zksnark.FiniteField.Ops._

/**
  * Barreto–Naehrig curve over some finite field
  * Curve equation:
  * Y^2^ = X^3^ + b, where "b" is a constant number belonging to corresponding specific field
  *
  * Code of curve arithmetic has been ported from:
  * <a href="https://github.com/scipr-lab/libff/blob/master/libff/algebra/curves/alt_bn128/alt_bn128_g1.cpp">bn128cpp</a>
  * and
  * <a href="https://github.com/ethereum/ethereumj/blob/develop/ethereumj-core/src/main/java/org/ethereum/crypto/zksnark/BN128.java">
  * bn128java
  * </a>
  */
sealed abstract class BN128[T: FiniteField] {
  val zero = Point(FiniteField[T].zero, FiniteField[T].zero, FiniteField[T].zero)

  def Fp_B: T

  protected def createPointOnCurve(x: T, y: T): Option[Point[T]] = {
    if (x.isZero() && y.isZero())
      Some(zero)
    else {
      val point = Point(x, y, FiniteField[T].one)
      Some(point).filter(isValidPoint)
    }
  }

  def toAffineCoordinates(p1: Point[T]): Point[T] = {
    if (p1.isZero)
      Point(zero.x, FiniteField[T].one, zero.z)
    else {
      val zInv = p1.z.inversed()
      val zInvSquared = zInv.squared()
      val zInvMul = zInv * zInvSquared

      val ax = p1.x * zInvSquared
      val ay = p1.y * zInvMul
      Point(ax, ay, FiniteField[T].one)
    }
  }

  def toEthNotation(p1: Point[T]): Point[T] = {
    val affine = toAffineCoordinates(p1)

    if (affine.isZero)
      zero
    else
      affine
  }

  /**
    * Point is on curve when its coordinates (x, y) satisfy curve equation which in jacobian coordinates becomes
    * Y^2^ = X^3^ + b * Z^6^
    */
  def isOnCurve(p1: Point[T]): Boolean = {
    if (p1.isZero)
      true
    else {
      val z6 = (p1.z.squared() * p1.z).squared()
      val l = p1.y.squared()
      val r = (p1.x.squared() * p1.x) + (Fp_B * z6)
      l == r
    }
  }

  def add(p1: Point[T], p2: Point[T]): Point[T] = {
    if (p1.isZero)
      p2
    else if (p2.isZero)
      p1
    else {
      val z1Squared = p1.z.squared()
      val z2Squared = p2.z.squared()

      val u1 = p1.x * z2Squared
      val u2 = p2.x * z1Squared

      val z1Cubed = p1.z * z1Squared
      val z2Cubed = p2.z * z2Squared

      val s1 = p1.y * z2Cubed
      val s2 = p2.y * z1Cubed

      if (u1 == u2 && s1 == s2) {
        dbl(p1)
      } else {
        val h = u2 - u1
        val i = h.doubled().squared()
        val j = h * i
        val r = (s2 - s1).doubled()
        val v = u1 * i
        val zz = (p1.z + p2.z).squared() - z1Squared - z2Squared

        val x3 = r.squared() - j - v.doubled()
        val y3 = r * (v - x3) - (s1 * j).doubled()
        val z3 = zz * h

        Point(x3, y3, z3)
      }
    }
  }

  def dbl(p1: Point[T]): Point[T] = {
    if (p1.isZero)
      p1
    else {
      val a = p1.x.squared()
      val b = p1.y.squared()
      val c = b.squared()

      val d = ((p1.x + b).squared() - a - c).doubled()

      val e = a + a + a
      val f = e.squared()

      val x3 = f - (d + d)
      val y3 = e * (d - x3) - c.doubled().doubled().doubled()
      val z3 = (p1.y * p1.z).doubled()

      Point(x3, y3, z3)
    }

  }

  /**
    * Multiplication by scalar n is just addition n times e.g n * P = P + P + .. n times.
    * Faster algorithm is used here, which is known as:
    * <a href=https://en.wikipedia.org/wiki/Elliptic_curve_point_multiplication#Double-and-add>Double-and-add</a>
    */
  def mul(p1: Point[T], s: BigInt): Point[T] = {
    if (s == 0 || p1.isZero)
      zero
    else {
      var i = s.bitLength - 1
      var result = zero
      while (i >= 0) {
        result = dbl(result)
        if (s.testBit(i)) {
          result = add(result, p1)
        }
        i = i - 1
      }
      result
    }
  }

  def isValidPoint(p1: Point[T]): Boolean =
    p1.isValid && isOnCurve(p1)
}

object BN128Fp extends BN128[Fp] {
  val Fp_B = Fp.B_Fp

  def createPoint(xx: ByteString, yy: ByteString): Option[Point[Fp]] = {
    val x = Fp(xx)
    val y = Fp(yy)

    createPointOnCurve(x, y)
  }
}

object BN128Fp2 extends BN128[Fp2] {
  val Fp_B = Fp2.B_Fp2

  def createPoint(a: ByteString, b: ByteString, c: ByteString, d: ByteString): Option[Point[Fp2]] = {
    val x = Fp2(a, b)
    val y = Fp2(c, d)
    createPointOnCurve(x, y)
  }
}

object BN128 {
  case class Point[T: FiniteField](x: T, y: T, z: T) {

    def isZero: Boolean = z.isZero()

    def isValid: Boolean =
      x.isValid() && y.isValid() && z.isValid()
  }

  case class BN128G1(p: Point[Fp])
  object BN128G1 {

    /**
      * Constructs valid element of subgroup `G1`
      * To be valid element of subgroup, elements needs to be valid point (have valid coordinates in Fp_2 and to be on curve
      * Bn128 in Fp
      * @return [[scala.None]] if element is invald group element, [[io.iohk.ethereum.crypto.zksnark.BN128.BN128G1]]
      */
    def apply(xx: ByteString, yy: ByteString): Option[BN128G1] = {
      // Every element of our Fp is also element of subgroup G1
      BN128Fp.createPoint(xx, yy).map(new BN128G1(_))
    }
  }

  case class BN128G2(p: Point[Fp2])
  object BN128G2 {
    import BN128Fp2._

    /**
      * "r" order of cyclic subgroup
      */
    val R = BigInt("21888242871839275222246405745257275088548364400416034343698204186575808495617")

    private val negOneModR = (-BigInt(1)).mod(R)

    private def isGroupElement(p: Point[Fp2]): Boolean = {
      add(mul(p, negOneModR), p).isZero // -1 * p + p == 0
    }

    /**
      * Constructs valid element of subgroup `G2`
      * To be valid element of subgroup, elements needs to be valid point (have valid coordinates in Fp_2 and to be on curve
      * Bn128 in Fp_2) and fullfill the equation `-1 * p + p == 0`
      * @return [[scala.None]] if element is invald group element, [[io.iohk.ethereum.crypto.zksnark.BN128.BN128G2]]
      */
    def apply(a: ByteString, b: ByteString, c: ByteString, d: ByteString): Option[BN128G2] = {
      createPoint(a, b, c, d).flatMap { point =>
        if (isGroupElement(point))
          Some(BN128G2(point))
        else
          None
      }
    }

    def mulByP(p: Point[Fp2]): Point[Fp2] = {
      val rx = Fp2.TWIST_MUL_BY_P_X * Fp2.frobeniusMap(p.x, 1)
      val ry = Fp2.TWIST_MUL_BY_P_Y * Fp2.frobeniusMap(p.y, 1)
      val rz = Fp2.frobeniusMap(p.z, 1)
      Point(rx, ry, rz)
    }
  }
}
