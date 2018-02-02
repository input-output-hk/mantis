package io.iohk.ethereum.extvm

import io.iohk.ethereum.domain.{Address, UInt256}
import io.iohk.ethereum.vm

class Storage(
    val address: Address,
    val storage: Map[UInt256, UInt256],
    cache: StorageCache)
  extends vm.Storage[Storage] {

  def store(offset: UInt256, value: UInt256): Storage =
    new Storage(address, storage + (offset -> value), cache)

  def load(offset: UInt256): UInt256 =
    storage.getOrElse(offset, cache.getStorageData(address, offset))
}
