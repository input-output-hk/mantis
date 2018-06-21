package io.iohk.ethereum.crypto.zksnark

import akka.util.ByteString
import io.iohk.ethereum.utils.ByteUtils

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
