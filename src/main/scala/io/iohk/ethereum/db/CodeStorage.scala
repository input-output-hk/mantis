package io.iohk.ethereum.db

import akka.util.ByteString

class CodeStorage(val dataSource: DataSource) extends KeyValueStorage[ByteString, ByteString] {
  type T = CodeStorage

  override val namespace: Byte = Namespaces.CodeNamespace
  override def keySerializer: ByteString => Array[Byte] = _.toArray
  override def valueSerializer: ByteString => Array[Byte] = _.toArray
  override def valueDeserializer: Array[Byte] => ByteString = ByteString(_)

  def apply(dataSource: DataSource): CodeStorage = new CodeStorage(dataSource)
}
