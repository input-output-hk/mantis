package io.iohk.ethereum.domain

import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.storage
import io.iohk.ethereum.db.storage.BlockMetadata
import io.iohk.ethereum.db.storage.BlockMetadataStorage
import io.iohk.ethereum.db.storage.StorageTypes.BlockHash

/** Proxy with the BlockMetadataStorage
  * When calling a put method a call to commit() is needed in order to persist the data
  * @param blockMetadataStorage BlockMetadataStorage instance
  */
class BlockMetadataProxy(blockMetadataStorage: BlockMetadataStorage) {

  def getBlockMetadata(blockHash: BlockHash): Option[storage.BlockMetadata] = blockMetadataStorage.get(blockHash)

  def putBlockMetadata(blockHash: BlockHash, blockMetadata: BlockMetadata): DataSourceBatchUpdate =
    blockMetadataStorage.put(blockHash, blockMetadata)

  def getBlockIsExecuted(blockHash: BlockHash): Boolean =
    blockMetadataStorage.get(blockHash).exists(_.isExecuted)

  // note: if BlockMetadata class gets more fields then update this to fist to a get and then
  // update that object
  def putBlockIsExecuted(blockHash: BlockHash, isExecuted: Boolean): DataSourceBatchUpdate =
    blockMetadataStorage.put(blockHash, BlockMetadata(isExecuted = isExecuted))
}
