package io.iohk.ethereum.vm

import akka.util.ByteString

import scala.math.ScalaNumericConversions


object DataWord {

  // TODO: Should be configurable
  val MaxLength = 32

  val MaxWord: BigInt = BigInt(2).pow(MaxLength * 8) - 1

  def apply(value: ByteString): DataWord = {
    require(value.length <= MaxLength, s"Input byte array cannot be longer than $MaxLength: ${value.length}")
    DataWord(value.foldLeft(BigInt(0)){(n, b) => (n << 8) + (b & 0xff)})
  }

  def apply(n: BigInt): DataWord = {
    new DataWord(fixBigInt(n))
  }

  def fixBigInt(n: BigInt): BigInt = {
    if (n == -MaxWord) {
      0
    } else if (n < 0) {
      n % MaxWord + MaxWord
    } else {
      n % MaxWord
    }
  }

}

/** Stores byte arrays of at most MaxLength size and adds a few convenience methods **/
class DataWord private (private val n: BigInt) extends ScalaNumericConversions {

  import DataWord._

  // TODO: Consider changing internal representation of a DataWord to a ByteString.
  val value: ByteString = ByteString(n.toByteArray.dropWhile(_ == 0))

  require(n >= 0 && n <= MaxWord, s"Invalid word value: $n")

  def &(that: DataWord): DataWord = DataWord(this.n & that.n)

  def |(that: DataWord): DataWord = DataWord(this.n | that.n)

  def ^(that: DataWord): DataWord = DataWord(this.n ^ that.n)

  def unary_-(): DataWord = DataWord(-this.n)

  def +(that: DataWord): DataWord = DataWord(this.n + that.n)

  def -(that: DataWord): DataWord = DataWord(this.n - that.n)

  def *(that: DataWord): DataWord = DataWord(this.n * that.n)

  def /(that: DataWord): DataWord = DataWord(this.n / that.n)

  def doubleValue(): Double = longValue().toDouble

  def floatValue(): Float = longValue().toFloat

  def intValue(): Int = value.foldLeft(0) { (n, b) => (n << 8) + (b & 0xff) }

  def longValue(): Long = value.foldLeft(0l) { (n, b) => (n << 8) + (b & 0xff) }

  def isWhole(): Boolean = true

  def underlying(): AnyRef = value

  override def hashCode(): Int = n.hashCode()

  override def equals(that: Any): Boolean =
    that match {
      case that: DataWord =>
        that.isInstanceOf[DataWord] && this.hashCode == that.hashCode
      case _ => false
    }

  override def toString(): String = {
    s"[$value, l: ${value.length}, BigInt($n)]"
  }

}
