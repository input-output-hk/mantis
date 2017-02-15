package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource

class BlockHeadersNumbersStorage(val dataSource: DataSource) extends KeyValueStorage[BigInt, ByteString] {
  override type T = BlockHeadersNumbersStorage
  override val namespace: IndexedSeq[Byte] = Namespaces.HeaderNumberNamespace

  override def keySerializer: (BigInt) => IndexedSeq[Byte] = _.toByteArray

  override def valueSerializer: (ByteString) => IndexedSeq[Byte] = identity

  override def valueDeserializer: (IndexedSeq[Byte]) => ByteString = a => ByteString(a: _*)

  override protected def apply(dataSource: DataSource): BlockHeadersNumbersStorage = new BlockHeadersNumbersStorage(dataSource)
}
