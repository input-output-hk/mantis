package io.iohk.ethereum.crypto.zksnark

import akka.util.ByteString
import io.iohk.ethereum.utils.ByteUtils
import io.iohk.ethereum.crypto.zksnark.FiniteField.Ops._

// Arithmetic in on all finite fields described in:
// https://eprint.iacr.org/2010/354.pdf - 'High-Speed Software Implementation of the Optimal Ate Pairing over Barreto–Naehrig Curves'
sealed abstract class FieldElement

case class Fp(inner: BigInt) extends FieldElement

object Fp {

  /**
    * "p" field parameter of F_p, F_p2, F_p6 and F_p12
    */
  val P: BigInt = BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208583")

  /**
    * "b" curve parameter for BN128Fp
    */
  val B_Fp: Fp = Fp(BigInt(3))

  val NON_RESIDUE = Fp(BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208582"))

  def apply(inner: ByteString): Fp = {
    new Fp(ByteUtils.toBigInt(inner))
  }

  /**
    * Implementation of finite field "Fp" modular arithmetic
    */
  implicit object FpImpl extends FiniteField[Fp] {
    override def zero: Fp = Fp(BigInt(0))

    override def one: Fp = Fp(BigInt(1))

    override def add(a: Fp, b: Fp): Fp = Fp {
      (a.inner + b.inner) mod P
    }

    override def mul(a: Fp, b: Fp): Fp = Fp {
      (a.inner * b.inner) mod P
    }

    override def sub(a: Fp, b: Fp): Fp = Fp {
      (a.inner - b.inner) mod P
    }

    override def inv(a: Fp): Fp = Fp {
      a.inner.modInverse(P)
    }

    override def neg(a: Fp): Fp = Fp {
      (-a.inner) mod P
    }

    override def isValid(a: Fp): Boolean = {
      a.inner >= 0 && a.inner < P
    }

    override def isZero(a: Fp): Boolean =
      a.inner.compareTo(BigInt(0)) == 0
  }
}

case class Fp2(a: Fp, b: Fp) extends FieldElement

object Fp2 {
  val NON_RESIDUE = Fp2(Fp(BigInt(9)), Fp(BigInt(1)))

  implicit object Fp2Impl extends FiniteField[Fp2] {
    override def one: Fp2 = Fp2(FiniteField[Fp].one, FiniteField[Fp].zero)

    override def zero: Fp2 = Fp2(FiniteField[Fp].zero, FiniteField[Fp].zero)

    override def add(a: Fp2, b: Fp2): Fp2 = {
      Fp2 (a.a + b.a, a.b + b.b)
    }

    override def sub(a: Fp2, b: Fp2): Fp2 = {
      Fp2(a.a - b.a, a.b - b.b)
    }

    override def mul(a: Fp2, b: Fp2): Fp2 = {
      val aa = a.a * b.a
      val bb = a.b * b.b

      val ra = (bb * Fp.NON_RESIDUE) + aa
      val rb = (a.a + a.b) * (b.a + b.b) - aa - bb

      Fp2(ra, rb)
    }

    override def neg(a: Fp2): Fp2 = {
      Fp2(a.a.negated(), a.b.negated())
    }

    override def inv(a: Fp2): Fp2 = {
      val t0 = a.a.squared()
      val t1 = a.b.squared()
      val t2 = t0 - (Fp.NON_RESIDUE * t1)
      val t3 = t2.inversed()

      val ra = a.a * t3
      val rb = (a.b * t3).negated()

      Fp2(ra, rb)
    }

    override def isValid(a: Fp2): Boolean = {
      a.a.isValid() && a.b.isValid()
    }

    override def isZero(a: Fp2): Boolean = a == zero

  }
}

case class Fp6(a: Fp2, b: Fp2, c: Fp2) extends FieldElement

object Fp6 {

  // Alg.12 in 'High-Speed Software Implementation of the Optimal Ate Pairing over Barreto–Naehrig Curves'
  def mulByNonResidue(a: Fp6): Fp6 = {
    val ra = Fp2.NON_RESIDUE * a.c
    val rb = a.a
    val rc = a.b

    Fp6(ra, rb, rc)
  }

  implicit object Fp6Impl extends FiniteField[Fp6] {

    override def one: Fp6 = Fp6(FiniteField[Fp2].one, FiniteField[Fp2].zero, FiniteField[Fp2].zero)

    override def zero: Fp6 = Fp6(FiniteField[Fp2].zero, FiniteField[Fp2].zero, FiniteField[Fp2].zero)

    override def add(a: Fp6, b: Fp6): Fp6 = {
      val ra = a.a + b.a
      val rb = a.b + b.b
      val rc = a.c + b.c
      Fp6(ra, rb,rc)
    }

    override def sub(a: Fp6, b: Fp6): Fp6 = {
      val ra = a.a - b.a
      val rb = a.b - b.b
      val rc = a.c - b.c
      Fp6(ra, rb,rc)
    }

    override def mul(a: Fp6, b: Fp6): Fp6 = {
      val t0 = a.a * b.a
      val t1 = a.b * b.b
      val t2 = a.c * b.c

      val ra = (((a.b + a.c) * (b.b + b.c) - t1 - t2) * Fp2.NON_RESIDUE) + t0
      val rb = (a.a + a.b) * (b.a + b.b) - t0 - t1 + (t2 * Fp2.NON_RESIDUE)
      val rc = (a.a + a.c) * (b.a + b.c) - t0 + t1 - t2

      Fp6(ra, rb, rc)
    }

    override def neg(a: Fp6): Fp6 = Fp6(a.a.negated(), a.b.negated(), a.c.negated())

    override def inv(a: Fp6): Fp6 = {
      val t0 = a.a.squared()
      val t1 = a.b.squared()
      val t2 = a.c.squared()
      val t3 = a.a * a.b
      val t4 = a.a * a.c
      val t5 = a.b * a.c

      val c0 = t0 - (t5 * Fp2.NON_RESIDUE)
      val c1 = (t2 * Fp2.NON_RESIDUE) - t3
      val c2 =  t1 - t4

      val t6 = ((a.a * c0) + ((a.c * c1 + a.b * c2) * Fp2.NON_RESIDUE)).inversed()

      val ra = c0 * t6
      val rb = c1 * t6
      val rc = c2 * t6

      Fp6(ra, rb, rc)
    }

    override def isValid(a: Fp6): Boolean = a.a.isValid() && a.b.isValid() && a.c.isValid()

    override def isZero(a: Fp6): Boolean = a == zero
  }
}

case class Fp12(a: Fp6, b: Fp6) extends FieldElement

object Fp12 {

  def cyclotomicSquared(a: Fp12): Fp12 = {
    var z0 = a.a.a
    var z4 = a.a.b
    var z3 = a.a.c
    var z2 = a.b.a
    var z1 = a.b.b
    var z5 = a.b.c

    var tmp = z0 * z1
    val t0 = (z0 + z1) * (z1 * Fp2.NON_RESIDUE + z0) - tmp - tmp * Fp2.NON_RESIDUE
    val t1 = tmp + tmp

    tmp = z2 * z3
    val t2 = (z2 + z3) * (z3 * Fp2.NON_RESIDUE + z2) - tmp - tmp * Fp2.NON_RESIDUE
    val t3 = tmp + tmp

    tmp = z4 * z5
    val t4 = (z4 + z5) * (z5 * Fp2.NON_RESIDUE + z4) - tmp - tmp * Fp2.NON_RESIDUE
    val t5 = tmp + tmp

    z0 = t0 - z0
    z0 = z0 + z0
    z0 = z0 + t0
    
    z1 = t1 + z1
    z1 = z1 + z1
    z1 = z1 + t1
    
    tmp = t5  * Fp2.NON_RESIDUE
    z2 = tmp + z2
    z2 = z2 + z2
    z2 = z2 + tmp
    
    z3 = t4 - z3
    z3 = z3 + z3
    z3 = z3 + t4
    
    z4 = t2 - z4
    z4 = z4 + z4
    z4 = z4 + t2
    
    z5 = t3 + z5
    z5 = z5 + z5
    z5 = z5 + t3

    Fp12(Fp6(z0, z4, z3), Fp6(z2, z1, z5))
  }

  // scalastyle:off method.length
  def mulBy024(a: Fp12, ell0: Fp2, ellVw: Fp2, ellVv: Fp2): Fp12 = {
    var z0 = a.a.a
    var z1 = a.a.b
    var z2 = a.a.c
    var z3 = a.b.a
    var z4 = a.b.b
    var z5 = a.b.c

    val x0 = ell0
    val x2 = ellVv
    val x4 = ellVw


    val d0 = z0 * x0
    val d2 = z2 * x2
    val d4 = z4 * x4
    val t2 = z0 + z4
    var t1 = z0 + z2
    val s0 = z1 + z3 + z5

    var s1 = z1 * x2
    var t3 = s1 + d4
    var t4 = t3 * Fp2.NON_RESIDUE + d0
    z0 = t4

    t3 = z5 * x4
    s1 = s1 + t3
    t3 = t3 + d2
    t4 = t3 * Fp2.NON_RESIDUE
    t3 = z1 * x0
    s1 = s1 + t3
    t4 = t4 + t3
    z1 = t4

    var t0 = x0 + x2
    t3 = t1 * t0 - d0 - d2
    t4 = z3 * x4
    s1 = s1 + t4
    t3 = t3 + t4

    t0 = z2 + z4
    z2 = t3

    t1 = x2 + x4
    t3 = t0 * t1 - d2 - d4
    t4 = t3 * Fp2.NON_RESIDUE
    t3 = z3 * x0
    s1 = s1 + t3
    t4 = t4 + t3
    z3 = t4

    t3 = z5 * x2
    s1 = s1 + t3
    t4 = t3 * Fp2.NON_RESIDUE
    t0 = x0 + x4
    t3 = t2 * t0 - d0 - d4
    t4 = t4 + t3
    z4 = t4

    t0 = x0 + x2 + x4
    t3 = s0 * t0 - s1
    z5 = t3

    Fp12(Fp6(z0, z1, z2), Fp6(z3, z4, z5))
  }

  def cyclotomicExp(a: Fp12, exp: BigInt): Fp12 = {
    var result = Fp12Impl.one
    var i = exp.bitLength - 1

    while(i >= 0) {
      result = cyclotomicSquared(result)

      if(exp.testBit(i)) {
        result = result * a
      }

      i = i - 1
    }

    result
  }

  def unitaryInverse(a: Fp12): Fp12 =
    Fp12(a.a, a.b.negated())

  def negExp(a: Fp12, exp: BigInt): Fp12 =
    unitaryInverse(cyclotomicExp(a, exp))


  val pairingFinalExp = BigInt("4965661367192848881")

  implicit object Fp12Impl extends FiniteField[Fp12] {
    override def one: Fp12 = Fp12(FiniteField[Fp6].one, FiniteField[Fp6].zero)

    override def zero: Fp12 = Fp12(FiniteField[Fp6].zero, FiniteField[Fp6].zero)

    override def add(a: Fp12, b: Fp12): Fp12 = Fp12(a.a + b.a, a.b + b.b)

    override def sub(a: Fp12, b: Fp12): Fp12 = Fp12(a.a - b.a, a.b - b.b)

    override def mul(a: Fp12, b: Fp12): Fp12 = {
      val t0 = a.a * b.a
      val t1 = a.b * b.b
      val c0 = t0 + Fp6.mulByNonResidue(t1)
      val c1 = (a.a + a.b) * (b.a + b.b) - t0 - t1
      Fp12(c0, c1)
    }

    override def neg(a: Fp12): Fp12 = Fp12(a.a.negated(), a.b.negated())

    override def inv(a: Fp12): Fp12 = {
      val t0 = a.a.squared()
      val t1 = a.b.squared()

      val t2 = t0 - Fp6.mulByNonResidue(t1)
      val t3 = t2.inversed()

      val ra = a.a * t3
      val rb = (a.b * t3).negated()

      Fp12(ra, rb)
    }

    override def isValid(a: Fp12): Boolean = a.a.isValid() && a.b.isValid()

    override def isZero(a: Fp12): Boolean = a == zero
  }
}