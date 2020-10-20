package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.Default.{Pickle, Unpickle}
import boopickle.DefaultBasic._
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes
import io.iohk.ethereum.utils.Picklers._

/**
  * This class is used to store the BlockHeader, by using:
  *   Key: hash of the block to which the BlockHeader belong
  *   Value: the block header
  */
class BlockHeadersStorage(val dataSource: DataSource)
    extends TransactionalKeyValueStorage[BlockHeaderHash, BlockHeader] {

  import BlockHeadersStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.HeaderNamespace

  override def keySerializer: BlockHeaderHash => IndexedSeq[Byte] = _.toIndexedSeq

  override def keyDeserializer: IndexedSeq[Byte] => BlockHeaderHash = k => ByteString.fromArrayUnsafe(k.toArray)

  override def valueSerializer: BlockHeader => IndexedSeq[Byte] =
    blockHeader => compactPickledBytes(Pickle.intoBytes(blockHeader))

  override def valueDeserializer: IndexedSeq[Byte] => BlockHeader =
    bytes => Unpickle[BlockHeader].fromBytes(ByteBuffer.wrap(bytes.toArray[Byte]))
}

object BlockHeadersStorage {
  type BlockHeaderHash = ByteString
}
