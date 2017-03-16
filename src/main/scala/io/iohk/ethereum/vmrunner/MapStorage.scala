package io.iohk.ethereum.vmrunner

import io.iohk.ethereum.vm.{UInt256, Storage}

object MapStorage {
  val Empty = MapStorage()
}

case class MapStorage(data: Map[UInt256, UInt256] = Map()) extends Storage[MapStorage] {
  def store(offset: UInt256, value: UInt256): MapStorage = {
    val updated =
      if (value == UInt256.Zero)
        data - offset
      else
        data + (offset -> value)

    copy(data = updated)
  }

  def load(offset: UInt256): UInt256 =
    data.getOrElse(offset, UInt256.Zero)
}
