package io.iohk.ethereum.vm

object DataWord {
  val MaxWord = BigInt(2).pow(256) - 1

  def fromBigInt(value: BigInt): DataWord =
    DataWord(if (value < 0) value % MaxWord + MaxWord else value % MaxWord)
}

case class DataWord(value: BigInt) {
  require(value >= 0 && value <= DataWord.MaxWord, s"Invalid word value: $value")
}
