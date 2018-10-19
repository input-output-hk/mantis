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
  def keySerializer: CodeHash => Array[Byte] = _.toArray[Byte]
  def valueSerializer: Code => Array[Byte] = _.toArray[Byte]
  def valueDeserializer: Array[Byte] => Code = (code: Array[Byte]) => ByteString(code)

  protected def apply(dataSource: DataSource): EvmCodeStorage = new EvmCodeStorage(dataSource)
}

object EvmCodeStorage {
  type CodeHash = ByteString
  type Code = ByteString
}
