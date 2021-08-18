package io.iohk.ethereum.db.storage

import akka.util.ByteString

import boopickle.Default.Pickle
import boopickle.Default.Unpickle

import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.StorageTypes.BlockBodyHash
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.utils.ByteUtils.byteSequenceToBuffer
import io.iohk.ethereum.utils.ByteUtils.compactPickledBytes
import io.iohk.ethereum.utils.Picklers._

/** This class is used to store the BlockBody, by using:
  *   Key: hash of the block to which the BlockBody belong
  *   Value: the block body
  */
class BlockBodiesStorage(val dataSource: DataSource) extends TransactionalKeyValueStorage[BlockBodyHash, BlockBody] {

  override val namespace: IndexedSeq[Byte] = Namespaces.BodyNamespace

  override def keySerializer: BlockBodyHash => IndexedSeq[Byte] = _.toIndexedSeq

  override def keyDeserializer: IndexedSeq[Byte] => BlockBodyHash = k => ByteString.fromArrayUnsafe(k.toArray)

  override def valueSerializer: BlockBody => IndexedSeq[Byte] = blockBody =>
    compactPickledBytes(Pickle.intoBytes(blockBody))

  override def valueDeserializer: IndexedSeq[Byte] => BlockBody =
    (byteSequenceToBuffer _).andThen(Unpickle[BlockBody].fromBytes)
}
