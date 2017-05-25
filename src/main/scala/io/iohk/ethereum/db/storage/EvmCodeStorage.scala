package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.EvmCodeStorage._

/**
  * This class is used to store the EVM Code, by using:
  *   Key: hash of the code
  *   Value: the code
  */
class EvmCodeStorage(val dataSource: DataSource) extends KeyValueStorage[CodeHash, Code, EvmCodeStorage] {
  val namespace: IndexedSeq[Byte] = Namespaces.CodeNamespace
  def keySerializer: CodeHash => IndexedSeq[Byte] = identity
  def valueSerializer: Code => IndexedSeq[Byte] = identity
  def valueDeserializer: IndexedSeq[Byte] => Code = (code: IndexedSeq[Byte]) => ByteString(code.toArray)

  protected def apply(dataSource: DataSource): EvmCodeStorage = new EvmCodeStorage(dataSource)
}

object EvmCodeStorage {
  type CodeHash = ByteString
  type Code = ByteString
}
