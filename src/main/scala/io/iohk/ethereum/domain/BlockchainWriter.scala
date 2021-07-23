package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.BlockBodiesStorage
import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.db.storage.ChainWeightStorage
import io.iohk.ethereum.db.storage.ReceiptStorage
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.db.storage.TransactionMappingStorage
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.domain.appstate.BestBlockInfo
import io.iohk.ethereum.utils.Logger

class BlockchainWriter(
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    blockNumberMappingStorage: BlockNumberMappingStorage,
    transactionMappingStorage: TransactionMappingStorage,
    receiptStorage: ReceiptStorage,
    chainWeightStorage: ChainWeightStorage,
    stateStorage: StateStorage,
    appStateStorage: AppStateStorage,
    blockchainMetadata: BlockchainMetadata
) extends Logger {

  def save(block: Block, receipts: Seq[Receipt], weight: ChainWeight, saveAsBestBlock: Boolean): Unit = {
    if (saveAsBestBlock && block.hasCheckpoint) {
      log.debug(
        "New best known block number - {}, new best checkpoint number - {}",
        block.header.number,
        block.header.number
      )
      saveBestKnownBlockAndLatestCheckpointNumber(block.header.hash, block.header.number, block.header.number)
    } else if (saveAsBestBlock) {
      log.debug(
        "New best known block number - {}",
        block.header.number
      )
      saveBestKnownBlock(block.header.hash, block.header.number)
    }

    log.debug("Saving new block {} to database", block.idTag)
    storeBlock(block)
      .and(storeReceipts(block.header.hash, receipts))
      .and(storeChainWeight(block.header.hash, weight))
      .commit()

    // not transactional part
    // the best blocks data will be persisted only when the cache will be persisted
    stateStorage.onBlockSave(block.header.number, appStateStorage.getBestBlockNumber())(persistBestBlocksData)
  }

  def storeReceipts(blockHash: ByteString, receipts: Seq[Receipt]): DataSourceBatchUpdate =
    receiptStorage.put(blockHash, receipts)

  def storeChainWeight(blockHash: ByteString, weight: ChainWeight): DataSourceBatchUpdate =
    chainWeightStorage.put(blockHash, weight)

  private def saveBestKnownBlock(bestBlockHash: ByteString, bestBlockNumber: BigInt): Unit =
    blockchainMetadata.bestKnownBlockAndLatestCheckpoint.updateAndGet(v =>
      v.copy(bestBlockInfo = BestBlockInfo(bestBlockHash, bestBlockNumber))
    )

  /** Persists a block in the underlying Blockchain Database
    * Note: all store* do not update the database immediately, rather they create
    * a [[io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate]] which then has to be committed (atomic operation)
    *
    * @param block Block to be saved
    */
  def storeBlock(block: Block): DataSourceBatchUpdate =
    storeBlockHeader(block.header).and(storeBlockBody(block.header.hash, block.body))

  def storeBlockHeader(blockHeader: BlockHeader): DataSourceBatchUpdate = {
    val hash = blockHeader.hash
    blockHeadersStorage.put(hash, blockHeader).and(saveBlockNumberMapping(blockHeader.number, hash))
  }

  def storeBlockBody(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate =
    blockBodiesStorage.put(blockHash, blockBody).and(saveTxsLocations(blockHash, blockBody))

  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): DataSourceBatchUpdate =
    blockNumberMappingStorage.put(number, hash)

  private def saveTxsLocations(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate =
    blockBody.transactionList.zipWithIndex.foldLeft(transactionMappingStorage.emptyBatchUpdate) {
      case (updates, (tx, index)) =>
        updates.and(transactionMappingStorage.put(tx.hash, TransactionLocation(blockHash, index)))
    }

  private def saveBestKnownBlockAndLatestCheckpointNumber(
      bestBlockHash: ByteString,
      number: BigInt,
      latestCheckpointNumber: BigInt
  ): Unit =
    blockchainMetadata.bestKnownBlockAndLatestCheckpoint.set(
      BestBlockLatestCheckpointNumbers(BestBlockInfo(bestBlockHash, number), latestCheckpointNumber)
    )

  private def persistBestBlocksData(): Unit = {
    val currentBestBlockInfo = blockchainMetadata.bestKnownBlockAndLatestCheckpoint.get().bestBlockInfo
    val currentBestCheckpointNumber = blockchainMetadata.bestKnownBlockAndLatestCheckpoint.get().latestCheckpointNumber
    log.debug(
      "Persisting app info data into database. Persisted block number is {}. " +
        "Persisted checkpoint number is {}",
      currentBestBlockInfo.number,
      currentBestCheckpointNumber
    )

    appStateStorage
      .putBestBlockData(currentBestBlockInfo)
      .and(appStateStorage.putLatestCheckpointBlockNumber(currentBestCheckpointNumber))
      .commit()
  }
}

object BlockchainWriter {
  def apply(storages: BlockchainStorages, metadata: BlockchainMetadata): BlockchainWriter =
    new BlockchainWriter(
      storages.blockHeadersStorage,
      storages.blockBodiesStorage,
      storages.blockNumberMappingStorage,
      storages.transactionMappingStorage,
      storages.receiptStorage,
      storages.chainWeightStorage,
      storages.stateStorage,
      storages.appStateStorage,
      metadata
    )
}
