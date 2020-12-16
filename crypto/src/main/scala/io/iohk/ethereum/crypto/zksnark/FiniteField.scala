package io.iohk.ethereum.crypto.zksnark

trait FiniteField[A] {
  def zero: A
  def one: A
  def add(a: A, b: A): A
  def mul(a: A, b: A): A
  def sub(a: A, b: A): A
  def inv(a: A): A
  def neg(a: A): A
  def sqr(a: A): A = mul(a, a)
  def dbl(a: A): A = add(a, a)
  def isZero(a: A): Boolean
  def isValid(a: A): Boolean
}

object FiniteField {
  def apply[T](implicit field: FiniteField[T]): FiniteField[T] = field
  object Ops {
    implicit class FiniteFieldOps[T](t: T)(implicit F: FiniteField[T]) {
      def +(o: T): T = F.add(t, o)
      def *(o: T): T = F.mul(t, o)
      def -(o: T): T = F.sub(t, o)
      def doubled(): T = F.dbl(t)
      def squared(): T = F.sqr(t)
      def inversed(): T = F.inv(t)
      def negated(): T = F.neg(t)
      def isValid(): Boolean = F.isValid(t)
      def isZero(): Boolean = F.isZero(t)
    }
  }
}
