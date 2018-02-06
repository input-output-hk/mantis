package io.iohk.ethereum.extvm

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.vm

class Storage(
    val address: Address,
    val storage: Map[ByteString, ByteString],
    cache: StorageCache)
  extends vm.Storage[Storage] {

  def store(offset: ByteString, value: ByteString): Storage =
    new Storage(address, storage + (offset -> value), cache)

  def load(offset: ByteString): ByteString =
    storage.getOrElse(offset, cache.getStorageData(address, offset))
}
