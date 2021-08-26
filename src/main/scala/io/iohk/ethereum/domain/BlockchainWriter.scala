package io.iohk.ethereum.domain

import akka.util.ByteString

import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.BlockBodiesStorage
import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.db.storage.ChainWeightStorage
import io.iohk.ethereum.db.storage.ReceiptStorage
import io.iohk.ethereum.db.storage.TransactionMappingStorage
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.domain.appstate.BlockInfo
import io.iohk.ethereum.utils.Logger

class BlockchainWriter(
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    blockMetadataProxy: BlockMetadataProxy,
    blockNumberMappingStorage: BlockNumberMappingStorage,
    transactionMappingStorage: TransactionMappingStorage,
    receiptStorage: ReceiptStorage,
    chainWeightStorage: ChainWeightStorage,
    appStateStorage: AppStateStorage
) extends Logger {

  def save(block: Block, receipts: Seq[Receipt], weight: ChainWeight, saveAsBestBlock: Boolean): Unit = {
    val updateBestBlocks = if (saveAsBestBlock && block.hasCheckpoint) {
      log.debug(
        "New best known block number - {}, new best checkpoint number - {}",
        block.header.number,
        block.header.number
      )
      appStateStorage
        .putBestBlockInfo(BlockInfo(block.header.hash, block.header.number))
        .and(appStateStorage.putLatestCheckpointBlockNumber(block.header.number))
    } else if (saveAsBestBlock) {
      log.debug(
        "New best known block number - {}",
        block.header.number
      )
      appStateStorage.putBestBlockInfo(BlockInfo(block.header.hash, block.header.number))
    } else {
      appStateStorage.emptyBatchUpdate
    }

    log.debug("Saving new block {} to database", block.idTag)
    storeBlock(block)
      .and(storeReceipts(block.header.hash, receipts))
      .and(storeChainWeight(block.header.hash, weight))
      .and(updateBestBlocks)
      .and(blockMetadataProxy.putBlockIsExecuted(block.header.hash, isExecuted = true))
      .commit()
  }

  def storeReceipts(blockHash: ByteString, receipts: Seq[Receipt]): DataSourceBatchUpdate =
    receiptStorage.put(blockHash, receipts)

  def storeChainWeight(blockHash: ByteString, weight: ChainWeight): DataSourceBatchUpdate =
    chainWeightStorage.put(blockHash, weight)

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

  def saveBestKnownBlocks(
      bestBlockHash: ByteString,
      bestBlockNumber: BigInt,
      latestCheckpointNumber: Option[BigInt] = None
  ): Unit =
    latestCheckpointNumber match {
      case Some(number) =>
        saveBestKnownBlockAndLatestCheckpointNumber(bestBlockHash, bestBlockNumber, number)
      case None =>
        saveBestKnownBlock(bestBlockHash, bestBlockNumber)
    }

  private def saveBestKnownBlock(bestBlockHash: ByteString, bestBlockNumber: BigInt): Unit =
    appStateStorage.putBestBlockInfo(BlockInfo(bestBlockHash, bestBlockNumber)).commit()

  private def saveBestKnownBlockAndLatestCheckpointNumber(
      bestBlockHash: ByteString,
      number: BigInt,
      latestCheckpointNumber: BigInt
  ): Unit =
    appStateStorage
      .putBestBlockInfo(BlockInfo(bestBlockHash, number))
      .and(appStateStorage.putLatestCheckpointBlockNumber(latestCheckpointNumber))
      .commit()

  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): DataSourceBatchUpdate =
    blockNumberMappingStorage.put(number, hash)

  private def saveTxsLocations(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate =
    blockBody.transactionList.zipWithIndex.foldLeft(transactionMappingStorage.emptyBatchUpdate) {
      case (updates, (tx, index)) =>
        updates.and(transactionMappingStorage.put(tx.hash, TransactionLocation(blockHash, index)))
    }
}

object BlockchainWriter {
  def apply(storages: BlockchainStorages): BlockchainWriter =
    new BlockchainWriter(
      storages.blockHeadersStorage,
      storages.blockBodiesStorage,
      new BlockMetadataProxy(storages.blockMetadataStorage),
      storages.blockNumberMappingStorage,
      storages.transactionMappingStorage,
      storages.receiptStorage,
      storages.chainWeightStorage,
      storages.appStateStorage
    )
}
