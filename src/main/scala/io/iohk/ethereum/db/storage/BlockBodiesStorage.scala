package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockBodiesStorage.BlockBodyHash
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}
import io.iohk.ethereum.utils.Config

/**
  * This class is used to store the BlockBody, by using:
  *   Key: hash of the block to which the BlockBody belong
  *   Value: the block body
  */
class BlockBodiesStorage(val dataSource: DataSource) extends KeyValueStorage[BlockBodyHash, BlockBody] {
  override type T = BlockBodiesStorage
  override val namespace: IndexedSeq[Byte] = Namespaces.BodyNamespace

  override def keySerializer: (BlockBodyHash) => IndexedSeq[Byte] = identity

  override def valueSerializer: (BlockBody) => IndexedSeq[Byte] =
    (blockBody: BlockBody) => rlpEncode[BlockBody](blockBody).toIndexedSeq

  override def valueDeserializer: (IndexedSeq[Byte]) => BlockBody =
    (encodedBlockBody: IndexedSeq[Byte]) => rlpDecode[BlockBody](encodedBlockBody.toArray)

  override protected def apply(dataSource: DataSource): BlockBodiesStorage = new BlockBodiesStorage(dataSource)
}

object BlockBodiesStorage {
  type BlockBodyHash = ByteString
}
