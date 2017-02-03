package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource

import CodeStorage._

/**
  * This class is used to store the EVM Code, by using:
  *   Key: hash of the code
  *   Value: the code
  */
class CodeStorage(val dataSource: DataSource) extends KeyValueStorage[CodeHash, Code] {
  type T = CodeStorage

  val namespace: Byte = Namespaces.CodeNamespace
  def keySerializer: CodeHash => IndexedSeq[Byte] = identity
  def valueSerializer: Code => IndexedSeq[Byte] = identity
  def valueDeserializer: IndexedSeq[Byte] => Code = (code: IndexedSeq[Byte]) => ByteString(code.toArray)

  protected def apply(dataSource: DataSource): CodeStorage = new CodeStorage(dataSource)
}

object CodeStorage {
  type CodeHash = ByteString
  type Code = ByteString
}
