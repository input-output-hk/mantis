package io.iohk.ethereum.vm

import akka.util.ByteString
import scala.math.ScalaNumericConversions


object DataWord {

  val MaxLength = 32

  def apply(value: ByteString): DataWord = {
    require(value.length <= MaxLength, s"Input byte array  cannot be longer than $MaxLength: ${value.length}")
    DataWord(value.foldLeft(BigInt(0)) { (n, b) => (n << 8) + (b & 0xff) })
  }

}

// TODO: Change internal representation of a DataWord to a ByteString.
case class DataWord private (n: BigInt) extends ScalaNumericConversions {

  lazy val  value: ByteString = ByteString(n.toByteArray)

  def &(that: DataWord): DataWord = {
    DataWord(this.n & that.n)
  }

  def |(that: DataWord): DataWord = {
    DataWord(this.n | that.n)
  }

  def ^(that: DataWord): DataWord = {
    DataWord(this.n ^ that.n)
  }

  def unary_-(): DataWord = {
    DataWord(-this.n)
  }

  def +(that: DataWord): DataWord = {
    DataWord(this.n + that.n)
  }

  def -(that: DataWord): DataWord = {
    DataWord(this.n - that.n)
  }

  def *(that: DataWord): DataWord = {
    DataWord(this.n * that.n)
  }

  def /(that: DataWord): DataWord = {
    DataWord(this.n / that.n)
  }

  def doubleValue(): Double = longValue().toDouble

  def floatValue(): Float = longValue().toFloat

  def intValue(): Int = value.foldLeft(0) { (n, b) => (n << 8) + (b & 0xff) }

  def longValue(): Long = value.foldLeft(0l) { (n, b) => (n << 8) + (b & 0xff) }

  def isWhole(): Boolean = true

  def underlying(): AnyRef = value

}
