package io.iohk.ethereum.vm

case class Storage(underlying: Vector[DataWord] = Vector()) {
  def save(addr: Int, value: DataWord): Storage =
    copy(underlying.updated(addr, value))

  def load(addr: Int): DataWord =
    underlying(addr)
}
