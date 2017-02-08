package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeader
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}

/**
  * This class is used to store the BlockHeader, by using:
  *   Key: hash of the block to which the BlockHeader belong
  *   Value: the block header
  */
class BlockHeadersStorage(val dataSource: DataSource) extends KeyValueStorage[BlockHeaderHash, BlockHeader] {
  override type T = BlockHeadersStorage
  override val namespace: IndexedSeq[Byte] = Namespaces.HeaderNamespace

  override def keySerializer: (BlockHeaderHash) => IndexedSeq[Byte] = identity

  override def valueSerializer: (BlockHeader) => IndexedSeq[Byte] =
    (blockHeader: BlockHeader) => rlpEncode[BlockHeader](blockHeader).toIndexedSeq

  override def valueDeserializer: (IndexedSeq[Byte]) => BlockHeader =
    (encodedBlockHeader: IndexedSeq[Byte]) => rlpDecode[BlockHeader](encodedBlockHeader.toArray)

  override protected def apply(dataSource: DataSource): BlockHeadersStorage = new BlockHeadersStorage(dataSource)
}

object BlockHeadersStorage {
  type BlockHeaderHash = ByteString
}
