package io.iohk.ethereum.extvm

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.vm

class Storage(
    val address: Address,
    val storage: Map[BigInt, BigInt],
    cache: StorageCache)
  extends vm.Storage[Storage] {

  def store(offset: BigInt, value: BigInt): Storage =
    new Storage(address, storage + (offset -> value), cache)

  def load(offset: BigInt): BigInt =
    storage.getOrElse(offset, cache.getStorageData(address, offset))
}
