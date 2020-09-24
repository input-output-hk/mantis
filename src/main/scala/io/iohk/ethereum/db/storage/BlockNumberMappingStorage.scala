package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash

import scala.collection.compat.immutable.ArraySeq

class BlockNumberMappingStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BigInt, BlockHeaderHash] {
  override val namespace: IndexedSeq[Byte] = Namespaces.HeightsNamespace

  override def keySerializer: (BigInt) => IndexedSeq[Byte] = index => ArraySeq.unsafeWrapArray(index.toByteArray)

  override def valueSerializer: (BlockHeaderHash) => IndexedSeq[Byte] = identity

  override def valueDeserializer: (IndexedSeq[Byte]) => BlockHeaderHash = arr => ByteString(arr.toArray[Byte])
}
