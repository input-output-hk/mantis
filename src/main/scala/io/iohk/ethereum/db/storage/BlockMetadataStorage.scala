package io.iohk.ethereum.db.storage

import akka.util.ByteString

import boopickle.Default._

import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.StorageTypes.BlockHash
import io.iohk.ethereum.utils.ByteUtils.byteSequenceToBuffer
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes

case class BlockMetadata(isExecuted: Boolean)

class BlockMetadataStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BlockHash, BlockMetadata] {
  override val namespace: IndexedSeq[Byte] = Namespaces.HeightsNamespace

  override def keySerializer: BlockHash => IndexedSeq[Byte] = identity

  override def keyDeserializer: IndexedSeq[Byte] => BlockHash = bytes => ByteString(bytes.toArray[Byte])

  override def valueSerializer: BlockMetadata => IndexedSeq[Byte] =
    (Pickle.intoBytes[BlockMetadata] _).andThen(compactPickledBytes)

  override def valueDeserializer: IndexedSeq[Byte] => BlockMetadata =
    (byteSequenceToBuffer _).andThen(Unpickle[BlockMetadata].fromBytes)
}
