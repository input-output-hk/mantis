package io.iohk.ethereum.vm

case class Memory(underlying: Vector[DataWord] = Vector()) {
  def save(addr: Int, value: DataWord): Memory =
    copy(underlying.updated(addr, value))

  def load(addr: Int): DataWord =
    underlying(addr)
}
