package io.iohk.ethereum.vm

case class Storage(underlying: Vector[DataWord] = Vector()) {
  def save(addr: Int, value: DataWord): Storage = {
    val extended =
      if (addr >= underlying.length || underlying.isEmpty)
        underlying.padTo(addr + 1, DataWord(0))
      else
        underlying
    val updated = extended.updated(addr, value)
    copy(updated)
  }

  def load(addr: Int): DataWord =
    underlying(addr)
}
