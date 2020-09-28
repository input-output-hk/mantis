package io.iohk.ethereum.db.storage

import java.nio.ByteBuffer

import akka.util.ByteString
import boopickle.Default.{Pickle, Unpickle}
import io.iohk.ethereum.crypto.ECDSASignature
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash
import io.iohk.ethereum.domain.{BlockHeader, Checkpoint}
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes

/**
  * This class is used to store the BlockHeader, by using:
  *   Key: hash of the block to which the BlockHeader belong
  *   Value: the block header
  */
class BlockHeadersStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BlockHeaderHash, BlockHeader] {

  import BlockHeadersStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.HeaderNamespace

  override def keySerializer: BlockHeaderHash => IndexedSeq[Byte] = _.toIndexedSeq

  override def valueSerializer: BlockHeader => IndexedSeq[Byte] =
    blockHeader => compactPickledBytes(Pickle.intoBytes(blockHeader))

  override def valueDeserializer: IndexedSeq[Byte] => BlockHeader =
    bytes => Unpickle[BlockHeader].fromBytes(ByteBuffer.wrap(bytes.toArray[Byte]))
}

object BlockHeadersStorage {
  type BlockHeaderHash = ByteString

  import boopickle.DefaultBasic._

  implicit val byteStringPickler: Pickler[ByteString] = transformPickler[ByteString, Array[Byte]](ByteString(_))(_.toArray[Byte])
  implicit val ecdsaSignaturePickler: Pickler[ECDSASignature] = generatePickler[ECDSASignature]
  implicit val checkpointPickler: Pickler[Checkpoint] = generatePickler[Checkpoint]
  implicit val blockHeaderPickler: Pickler[BlockHeader] = generatePickler[BlockHeader]
}
