package io.iohk.ethereum.vmrunner

import io.iohk.ethereum.vm.{DataWord, Storage}

object MapStorage {
  val Empty = MapStorage()
}

case class MapStorage(data: Map[DataWord, DataWord] = Map()) extends Storage[MapStorage] {
  def store(addr: DataWord, value: DataWord): MapStorage = {
    val updated =
      if (value == DataWord.Zero)
        data - addr
      else
        data + (addr -> value)

    copy(data = updated)
  }

  def load(addr: DataWord): DataWord =
    data.getOrElse(addr, DataWord.Zero)
}
