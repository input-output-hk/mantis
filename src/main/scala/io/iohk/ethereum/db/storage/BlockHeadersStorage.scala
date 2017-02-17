package io.iohk.ethereum.db.storage

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSource
import io.iohk.ethereum.db.storage.BlockHeadersStorage.BlockHeaderHash
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.rlp.{decode => rlpDecode, encode => rlpEncode}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockHeaderImplicits._
import io.iohk.ethereum.utils.Config

/**
  * This class is used to store the BlockHeader, by using:
  *   Key: hash of the block to which the BlockHeader belong
  *   Value: the block header
  */
class BlockHeadersStorage(val dataSource: DataSource,
  val blockHeadersNumbersStorage: BlockHeadersNumbersStorage) extends KeyValueStorage[BlockHeaderHash, BlockHeader] {
  override type T = BlockHeadersStorage
  override val namespace: IndexedSeq[Byte] = Namespaces.HeaderNamespace

  override def keySerializer: (BlockHeaderHash) => IndexedSeq[Byte] = identity

  override def valueSerializer: (BlockHeader) => IndexedSeq[Byte] =
    (blockHeader: BlockHeader) => rlpEncode[BlockHeader](blockHeader).toIndexedSeq

  override def valueDeserializer: (IndexedSeq[Byte]) => BlockHeader =
    (encodedBlockHeader: IndexedSeq[Byte]) => rlpDecode[BlockHeader](encodedBlockHeader.toArray)

  override protected def apply(dataSource: DataSource): BlockHeadersStorage =
    new BlockHeadersStorage(dataSource, blockHeadersNumbersStorage)

  override def get(key: BlockHeaderHash): Option[BlockHeader] = if (key == Config.Blockchain.genesisBlockHeader.hash) {
    Some(Config.Blockchain.genesisBlockHeader)
  } else {
    super.get(key)
  }

  def get(blockNumber: BigInt): Option[BlockHeader] = if (blockNumber == 0) {
    Some(Config.Blockchain.genesisBlockHeader)
  } else {
    blockHeadersNumbersStorage.get(blockNumber).flatMap(key => super.get(key))
  }

  override def update(toRemove: Seq[BlockHeaderHash], toUpsert: Seq[(BlockHeaderHash, BlockHeader)]): BlockHeadersStorage = {
    val numbersToRemove = toRemove.map(get).collect { case Some(block) => block.number }
    val numberToUpdate = toUpsert.map { case (hash, block) => (block.number, hash) }
    blockHeadersNumbersStorage.update(numbersToRemove, numberToUpdate)
    super.update(toRemove, toUpsert)
  }

}

object BlockHeadersStorage {
  type BlockHeaderHash = ByteString
}
