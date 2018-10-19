package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash

class BlockNumberMappingStorage(val dataSource: DataSource) extends KeyValueStorage[BigInt, BlockHeaderHash, BlockNumberMappingStorage] {
  override val namespace: IndexedSeq[Byte] = Namespaces.HeightsNamespace

  override def keySerializer: (BigInt) => Array[Byte] = index => index.toByteArray

  override def valueSerializer: (BlockHeaderHash) => Array[Byte] = _.toArray[Byte]

  override def valueDeserializer: (Array[Byte]) => BlockHeaderHash = arr => ByteString(arr)

  override protected def apply(dataSource: DataSource): BlockNumberMappingStorage = new BlockNumberMappingStorage(dataSource)
}
