package io.iohk.ethereum.crypto.zksnark

import akka.util.ByteString
import io.iohk.ethereum.crypto.zksnark.BN128.Point
import io.iohk.ethereum.crypto.zksnark.FiniteField.Ops._

sealed abstract class BN128[T: FiniteField] {
  val zero = Point(FiniteField[T].zero, FiniteField[T].zero, FiniteField[T].zero)

  def Fp_B: T

  def createPointOnCurve(xx: ByteString, yy: ByteString)(implicit ev: FiniteField[T]): Option[Point[T]]

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

  def dbl(p1: Point[T]): Point[T] ={
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

class BN128Fp extends BN128[Fp] {
  val Fp_B = Fp.B_Fp

  override def createPointOnCurve(xx: ByteString, yy: ByteString)(implicit ev: FiniteField[Fp]): Option[Point[Fp]] = {
    val x = Fp(xx)
    val y = Fp(yy)

    if (x.isZero() && y.isZero())
      Some(zero)
    else {
      val point = Point(x, y, ev.one)
      Some(point).filter(isValidPoint)
    }
  }
}

object BN128 {
  case class Point[T: FiniteField](x: T, y: T, z: T) {

    def isZero: Boolean = z.isZero()

    def isValid: Boolean =
      x.isValid() && y.isValid() && z.isValid()
  }
}