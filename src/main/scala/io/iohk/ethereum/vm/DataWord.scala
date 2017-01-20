package io.iohk.ethereum.vm

import akka.util.ByteString
import scala.math.ScalaNumericConversions


object DataWord {

  val MaxWord = BigInt(2).pow(256) - 1

  val MaxLength = 32

  def apply(value: ByteString): DataWord = {
    require(value.length <= MaxLength, s"Input byte array cannot be longer than $MaxLength: ${value.length}")
    DataWord(value.foldLeft(BigInt(0)) { (n, b) => (n << 8) + (b & 0xff) })
  }

  private def apply(n: BigInt): DataWord = {
    if (n < 0) {
      new DataWord(n % MaxWord + MaxWord)
    } else {
      new DataWord(n % MaxWord)
    }
  }

}

// TODO: Change internal representation of a DataWord to a ByteString.
class DataWord private (private val n: BigInt) extends ScalaNumericConversions {

  import DataWord._

  require(n >= 0 && n <= MaxWord, s"Invalid word value: $n")

  lazy val value: ByteString = ByteString(n.toByteArray)

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

}
