package io.iohk.ethereum.vm

object Storage {

  val Zero = DataWord(0)

}

class Storage(val underlying: Map[DataWord, DataWord] = Map()) {

  import Storage.Zero

  def store(addr: DataWord, value: DataWord): Storage = new Storage(underlying + (addr -> value))

  def load(addr: DataWord): DataWord = underlying.getOrElse(addr, Zero)

}
