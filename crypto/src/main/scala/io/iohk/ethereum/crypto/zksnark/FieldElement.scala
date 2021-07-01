package io.iohk.ethereum.crypto.zksnark

import akka.util.ByteString

import io.iohk.ethereum.crypto.zksnark.FiniteField.Ops._
import io.iohk.ethereum.utils.ByteUtils

// Arithmetic in on all finite fields described in:
// https://eprint.iacr.org/2010/354.pdf - 'High-Speed Software Implementation of the Optimal Ate Pairing over Barreto–Naehrig Curves'
sealed abstract class FieldElement

case class Fp(inner: BigInt) extends FieldElement

object Fp {

  /** "p" field parameter of F_p, F_p2, F_p6 and F_p12
    */
  val P: BigInt = BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208583")

  /** "b" curve parameter for BN128Fp
    */
  val B_Fp: Fp = Fp(BigInt(3))

  val twoInv: Fp = Fp(BigInt(2).modInverse(P))

  val NON_RESIDUE: Fp = Fp(BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208582"))

  def apply(inner: ByteString): Fp =
    new Fp(ByteUtils.toBigInt(inner))

  /** Implementation of finite field "Fp" modular arithmetic
    */
  implicit object FpImpl extends FiniteField[Fp] {
    override def zero: Fp = Fp(BigInt(0))

    override def one: Fp = Fp(BigInt(1))

    override def add(a: Fp, b: Fp): Fp = Fp {
      (a.inner + b.inner).mod(P)
    }

    override def mul(a: Fp, b: Fp): Fp = Fp {
      (a.inner * b.inner).mod(P)
    }

    override def sub(a: Fp, b: Fp): Fp = Fp {
      (a.inner - b.inner).mod(P)
    }

    override def inv(a: Fp): Fp = Fp {
      a.inner.modInverse(P)
    }

    override def neg(a: Fp): Fp = Fp {
      (-a.inner).mod(P)
    }

    override def isValid(a: Fp): Boolean =
      a.inner >= 0 && a.inner < P

    override def isZero(a: Fp): Boolean =
      a.inner == BigInt(0)
  }
}

case class Fp2(a: Fp, b: Fp) extends FieldElement

object Fp2 {

  // It is also Twist for Fp2
  val NON_RESIDUE: Fp2 = Fp2(Fp(BigInt(9)), Fp(BigInt(1)))

  val TWIST_MUL_BY_P_X: Fp2 = Fp2(
    Fp(BigInt("21575463638280843010398324269430826099269044274347216827212613867836435027261")),
    Fp(BigInt("10307601595873709700152284273816112264069230130616436755625194854815875713954"))
  )

  val TWIST_MUL_BY_P_Y: Fp2 = Fp2(
    Fp(BigInt("2821565182194536844548159561693502659359617185244120367078079554186484126554")),
    Fp(BigInt("3505843767911556378687030309984248845540243509899259641013678093033130930403"))
  )

  def apply(inner1: ByteString, inner2: ByteString): Fp2 =
    new Fp2(Fp(inner1), Fp(inner2))

  def mulByConst(a: Fp2, c: Fp): Fp2 =
    Fp2(a.a * c, a.b * c)

  private val FROBENIUS_COEFFS_B: Array[Fp] = Array[Fp](
    FiniteField[Fp].one,
    Fp(BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208582"))
  )

  def frobeniusMap(a: Fp2, power: Int): Fp2 = {
    val ra = a.a
    val rb = FROBENIUS_COEFFS_B(power % 2) * a.b
    Fp2(ra, rb)
  }

  implicit object Fp2Impl extends FiniteField[Fp2] {
    override def one: Fp2 = Fp2(FiniteField[Fp].one, FiniteField[Fp].zero)

    override def zero: Fp2 = Fp2(FiniteField[Fp].zero, FiniteField[Fp].zero)

    override def add(a: Fp2, b: Fp2): Fp2 =
      Fp2(a.a + b.a, a.b + b.b)

    override def sub(a: Fp2, b: Fp2): Fp2 =
      Fp2(a.a - b.a, a.b - b.b)

    override def mul(a: Fp2, b: Fp2): Fp2 = {
      val aa = a.a * b.a
      val bb = a.b * b.b

      val ra = (bb * Fp.NON_RESIDUE) + aa
      val rb = (a.a + a.b) * (b.a + b.b) - aa - bb

      Fp2(ra, rb)
    }

    override def neg(a: Fp2): Fp2 =
      Fp2(a.a.negated(), a.b.negated())

    override def inv(a: Fp2): Fp2 = {
      val t0 = a.a.squared()
      val t1 = a.b.squared()
      val t2 = t0 - (Fp.NON_RESIDUE * t1)
      val t3 = t2.inversed()

      val ra = a.a * t3
      val rb = (a.b * t3).negated()

      Fp2(ra, rb)
    }

    override def isValid(a: Fp2): Boolean =
      a.a.isValid() && a.b.isValid()

    override def isZero(a: Fp2): Boolean = a == zero

  }

  val B_Fp2: Fp2 = mulByConst(NON_RESIDUE.inversed(), Fp.B_Fp)
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

  def frobeniusMap(a: Fp6, power: Int): Fp6 = {
    val ra = Fp2.frobeniusMap(a.a, power)
    val rb = FROBENIUS_COEFFS_B(power % 6) * Fp2.frobeniusMap(a.b, power)
    val rc = FROBENIUS_COEFFS_C(power % 6) * Fp2.frobeniusMap(a.c, power)

    Fp6(ra, rb, rc)
  }

  def mulByConst(a: Fp6, b: Fp2): Fp6 = {
    val ra = a.a * b
    val rb = a.b * b
    val rc = a.c * b
    Fp6(ra, rb, rc)
  }

  private val FROBENIUS_COEFFS_B: Array[Fp2] = Array[Fp2](
    new Fp2(
      FiniteField[Fp].one,
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("21575463638280843010398324269430826099269044274347216827212613867836435027261")),
      Fp(BigInt("10307601595873709700152284273816112264069230130616436755625194854815875713954"))
    ),
    new Fp2(
      Fp(BigInt("21888242871839275220042445260109153167277707414472061641714758635765020556616")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("3772000881919853776433695186713858239009073593817195771773381919316419345261")),
      Fp(BigInt("2236595495967245188281701248203181795121068902605861227855261137820944008926"))
    ),
    new Fp2(
      Fp(BigInt("2203960485148121921418603742825762020974279258880205651966")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("18429021223477853657660792034369865839114504446431234726392080002137598044644")),
      Fp(BigInt("9344045779998320333812420223237981029506012124075525679208581902008406485703"))
    )
  )

  private val FROBENIUS_COEFFS_C: Array[Fp2] = Array[Fp2](
    new Fp2(FiniteField[Fp].one, FiniteField[Fp].zero),
    new Fp2(
      Fp(BigInt("2581911344467009335267311115468803099551665605076196740867805258568234346338")),
      Fp(BigInt("19937756971775647987995932169929341994314640652964949448313374472400716661030"))
    ),
    new Fp2(
      Fp(BigInt("2203960485148121921418603742825762020974279258880205651966")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("5324479202449903542726783395506214481928257762400643279780343368557297135718")),
      Fp(BigInt("16208900380737693084919495127334387981393726419856888799917914180988844123039"))
    ),
    new Fp2(
      Fp(BigInt("21888242871839275220042445260109153167277707414472061641714758635765020556616")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("13981852324922362344252311234282257507216387789820983642040889267519694726527")),
      Fp(BigInt("7629828391165209371577384193250820201684255241773809077146787135900891633097"))
    )
  )

  implicit object Fp6Impl extends FiniteField[Fp6] {

    override def one: Fp6 = Fp6(FiniteField[Fp2].one, FiniteField[Fp2].zero, FiniteField[Fp2].zero)

    override def zero: Fp6 = Fp6(FiniteField[Fp2].zero, FiniteField[Fp2].zero, FiniteField[Fp2].zero)

    override def add(a: Fp6, b: Fp6): Fp6 = {
      val ra = a.a + b.a
      val rb = a.b + b.b
      val rc = a.c + b.c
      Fp6(ra, rb, rc)
    }

    override def sub(a: Fp6, b: Fp6): Fp6 = {
      val ra = a.a - b.a
      val rb = a.b - b.b
      val rc = a.c - b.c
      Fp6(ra, rb, rc)
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
      val c2 = t1 - t4

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

  private def cyclotomicSquared(a: Fp12): Fp12 = {
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

    tmp = t5 * Fp2.NON_RESIDUE
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

  private def cyclotomicExp(a: Fp12, exp: BigInt): Fp12 = {
    var result = Fp12Impl.one
    var i = exp.bitLength - 1

    while (i >= 0) {
      result = cyclotomicSquared(result)

      if (exp.testBit(i)) {
        result = result * a
      }

      i = i - 1
    }

    result
  }

  private def unitaryInverse(a: Fp12): Fp12 =
    Fp12(a.a, a.b.negated())

  def negExp(a: Fp12, exp: BigInt): Fp12 =
    unitaryInverse(cyclotomicExp(a, exp))

  def finalExp(el: Fp12): Fp12 =
    finalExpLastChunk(finalExpFirstChunk(el))

  private def finalExpFirstChunk(el: Fp12): Fp12 = {
    val a = unitaryInverse(el)
    val b = el.inversed()
    val c = a * b
    val d = frobeniusMap(c, 2)
    d * c
  }

  private def finalExpLastChunk(el: Fp12): Fp12 = {
    val a = negExp(el, pairingFinalExp)
    val b = cyclotomicSquared(a)
    val c = cyclotomicSquared(b)
    val d = c * b

    val e = negExp(d, pairingFinalExp)
    val f = cyclotomicSquared(e)
    val g = negExp(f, pairingFinalExp)
    val h = unitaryInverse(d)
    val i = unitaryInverse(g)

    val j = i * e
    val k = j * h
    val l = k * b
    val m = k * e
    val n = el * m

    val o = frobeniusMap(l, 1)
    val p = o * n

    val q = frobeniusMap(k, 2)
    val r = q * p

    val s = unitaryInverse(el)
    val t = s * l
    val u = frobeniusMap(t, 3)
    val v = u * r
    v
  }

  val pairingFinalExp: BigInt = BigInt("4965661367192848881")

  private val FROBENIUS_COEFFS_B: Array[Fp2] = Array[Fp2](
    new Fp2(FiniteField[Fp].one, FiniteField[Fp].zero),
    new Fp2(
      Fp(BigInt("8376118865763821496583973867626364092589906065868298776909617916018768340080")),
      Fp(BigInt("16469823323077808223889137241176536799009286646108169935659301613961712198316"))
    ),
    new Fp2(
      Fp(BigInt("21888242871839275220042445260109153167277707414472061641714758635765020556617")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("11697423496358154304825782922584725312912383441159505038794027105778954184319")),
      Fp(BigInt("303847389135065887422783454877609941456349188919719272345083954437860409601"))
    ),
    new Fp2(
      Fp(BigInt("21888242871839275220042445260109153167277707414472061641714758635765020556616")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("3321304630594332808241809054958361220322477375291206261884409189760185844239")),
      Fp(BigInt("5722266937896532885780051958958348231143373700109372999374820235121374419868"))
    ),
    new Fp2(
      Fp(BigInt("21888242871839275222246405745257275088696311157297823662689037894645226208582")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("13512124006075453725662431877630910996106405091429524885779419978626457868503")),
      Fp(BigInt("5418419548761466998357268504080738289687024511189653727029736280683514010267"))
    ),
    new Fp2(
      Fp(BigInt("2203960485148121921418603742825762020974279258880205651966")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("10190819375481120917420622822672549775783927716138318623895010788866272024264")),
      Fp(BigInt("21584395482704209334823622290379665147239961968378104390343953940207365798982"))
    ),
    new Fp2(
      Fp(BigInt("2203960485148121921418603742825762020974279258880205651967")),
      FiniteField[Fp].zero
    ),
    new Fp2(
      Fp(BigInt("18566938241244942414004596690298913868373833782006617400804628704885040364344")),
      Fp(BigInt("16165975933942742336466353786298926857552937457188450663314217659523851788715"))
    )
  )

  private def frobeniusMap(a: Fp12, power: Int): Fp12 = {
    val ra = Fp6.frobeniusMap(a.a, power)
    val rb = Fp6.mulByConst(Fp6.frobeniusMap(a.b, power), FROBENIUS_COEFFS_B(power % 12))
    Fp12(ra, rb)
  }

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
