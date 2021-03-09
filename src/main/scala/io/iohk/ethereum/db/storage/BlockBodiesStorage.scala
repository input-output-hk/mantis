package io.iohk.ethereum.db.storage

import akka.util.ByteString
import boopickle.Default.{Pickle, Unpickle}
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockBodiesStorage.BlockBodyHash
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.utils.ByteUtils.{byteSequenceToBuffer, compactPickledBytes}
import io.iohk.ethereum.utils.Picklers._

/** This class is used to store the BlockBody, by using:
  *   Key: hash of the block to which the BlockBody belong
  *   Value: the block body
  */
class BlockBodiesStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BlockBodyHash, BlockBody] {
  import BlockBodiesStorage._

  override val namespace: IndexedSeq[Byte] = Namespaces.BodyNamespace

  override def keySerializer: BlockBodyHash => IndexedSeq[Byte] = _.toIndexedSeq

  override def keyDeserializer: IndexedSeq[Byte] => BlockBodyHash = k => ByteString.fromArrayUnsafe(k.toArray)

  override def valueSerializer: BlockBody => IndexedSeq[Byte] = blockBody =>
    compactPickledBytes(Pickle.intoBytes(blockBody))

  override def valueDeserializer: IndexedSeq[Byte] => BlockBody =
    (byteSequenceToBuffer _).andThen(Unpickle[BlockBody].fromBytes)
}

object BlockBodiesStorage {
  type BlockBodyHash = ByteString
}
