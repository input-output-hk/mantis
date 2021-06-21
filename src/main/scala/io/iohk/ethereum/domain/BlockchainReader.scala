package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.db.storage.BlockHeadersStorage

class BlockchainReader(blockHeadersStorage: BlockHeadersStorage) {

  /**
    * Allows to query a blockHeader by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] =
    blockHeadersStorage.get(hash)

}
