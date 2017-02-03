package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource

import CodeStorage._

class CodeStorage(val dataSource: DataSource) extends KeyValueStorage[CodeHash, Code] {
  type T = CodeStorage

  val namespace: Byte = Namespaces.CodeNamespace
  def keySerializer: CodeHash => Array[Byte] = _.toArray
  def valueSerializer: Code => Array[Byte] = _.toArray
  def valueDeserializer: Array[Byte] => Code = ByteString(_)

  protected def apply(dataSource: DataSource): CodeStorage = new CodeStorage(dataSource)
}

object CodeStorage {
  type CodeHash = ByteString
  type Code = ByteString
}
