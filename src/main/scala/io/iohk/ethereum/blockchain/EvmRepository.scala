package io.iohk.ethereum.blockchain

import akka.util.ByteString
import io.iohk.ethereum.db.storage.EvmCodeStorage

trait EvmRepository {
  protected def evmCodeStorage: EvmCodeStorage

  def save(hash: ByteString, evmCode: ByteString): Unit = evmCodeStorage.put(hash, evmCode)

  def getEvmCodeByHash(hash: ByteString): Option[ByteString] = evmCodeStorage.get(hash)
}
