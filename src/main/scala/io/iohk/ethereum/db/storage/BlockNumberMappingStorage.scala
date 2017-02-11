package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash

class BlockNumberMappingStorage(val dataSource: DataSource) extends KeyValueStorage[BigInt, BlockHeaderHash] {
  override type T = BlockNumberMappingStorage
  override val namespace: IndexedSeq[Byte] = Namespaces.HeightsNamespace

  override def keySerializer: (BigInt) => IndexedSeq[Byte] = index => index.toByteArray

  override def valueSerializer: (BlockHeaderHash) => IndexedSeq[Byte] = identity

  override def valueDeserializer: (IndexedSeq[Byte]) => BlockHeaderHash = arr => ByteString(arr.toArray[Byte])

  override protected def apply(dataSource: DataSource): BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSource)
}
