package io.iohk.ethereum.domain

import akka.util.ByteString

import scala.annotation.tailrec

import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain
import io.iohk.ethereum.domain.appstate.BlockInfo
import io.iohk.ethereum.jsonrpc.ProofService.StorageProof
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.InMemoryWorldStateProxyStorage
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.utils.ByteStringUtils
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.vm.Storage
import io.iohk.ethereum.vm.WorldStateProxy

/** Entity to be used to persist and query  Blockchain related objects (blocks, transactions, ommers)
  */
trait Blockchain {

  type S <: Storage[S]
  type WS <: WorldStateProxy[WS, S]

  /** Get account storage at given position
    *
    * @param rootHash storage root hash
    * @param position storage position
    */
  def getAccountStorageAt(rootHash: ByteString, position: BigInt, ethCompatibleStorage: Boolean): ByteString

  /** Get a storage-value and its proof being the path from the root node until the last matching node.
    *
    * @param rootHash storage root hash
    * @param position storage position
    */
  def getStorageProofAt(
      rootHash: ByteString,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): StorageProof

  /** Get the MptStorage
    * @param blockNumber
    * @return MptStorage
    */
  def getBackingMptStorage(blockNumber: BigInt): MptStorage

  /** Get the MptStorage for read-only
    *
    * @return MptStorage
    */
  def getReadOnlyMptStorage(): MptStorage

  def getLatestCheckpointBlockNumber(): BigInt

  def removeBlock(hash: ByteString): Unit

  def saveBestKnownBlocks(
      bestBlockHash: ByteString,
      bestBlockNumber: BigInt,
      latestCheckpointNumber: Option[BigInt] = None
  ): Unit

}

class BlockchainImpl(
    protected val blockHeadersStorage: BlockHeadersStorage,
    protected val blockBodiesStorage: BlockBodiesStorage,
    protected val blockNumberMappingStorage: BlockNumberMappingStorage,
    protected val receiptStorage: ReceiptStorage,
    protected val chainWeightStorage: ChainWeightStorage,
    protected val transactionMappingStorage: TransactionMappingStorage,
    protected val appStateStorage: AppStateStorage,
    protected val stateStorage: StateStorage,
    blockchainReader: BlockchainReader
) extends Blockchain
    with Logger {

  override def getLatestCheckpointBlockNumber(): BigInt = appStateStorage.getLatestCheckpointBlockNumber()

  override def getAccountStorageAt(
      rootHash: ByteString,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): ByteString = {
    val storage = stateStorage.getBackingStorage(0)
    val mpt =
      if (ethCompatibleStorage) domain.EthereumUInt256Mpt.storageMpt(rootHash, storage)
      else domain.ArbitraryIntegerMpt.storageMpt(rootHash, storage)

    val bigIntValue = mpt.get(position).getOrElse(BigInt(0))
    val byteArrayValue = bigIntValue.toByteArray

    // BigInt.toArray actually might return one more byte than necessary because it adds a sign bit, which in our case
    // will always be 0. This would add unwanted 0 bytes and might cause the value to be 33 byte long while an EVM
    // word is 32 byte long.
    if (bigIntValue != 0)
      ByteString(byteArrayValue.dropWhile(_ == 0))
    else
      ByteString(byteArrayValue)
  }

  override def getStorageProofAt(
      rootHash: ByteString,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): StorageProof = {
    val storage: MptStorage = stateStorage.getBackingStorage(0)
    val mpt: MerklePatriciaTrie[BigInt, BigInt] = {
      if (ethCompatibleStorage) domain.EthereumUInt256Mpt.storageMpt(rootHash, storage)
      else domain.ArbitraryIntegerMpt.storageMpt(rootHash, storage)
    }
    val value: Option[BigInt] = mpt.get(position)
    val proof: Option[Vector[MptNode]] = mpt.getProof(position)
    StorageProof(position, value, proof)
  }

  def getBackingMptStorage(blockNumber: BigInt): MptStorage = stateStorage.getBackingStorage(blockNumber)

  def getReadOnlyMptStorage(): MptStorage = stateStorage.getReadOnlyStorage

  override def saveBestKnownBlocks(
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

  private def removeBlockNumberMapping(number: BigInt): DataSourceBatchUpdate =
    blockNumberMappingStorage.remove(number)

  override def removeBlock(blockHash: ByteString): Unit = {
    val maybeBlock = blockchainReader.getBlockByHash(blockHash)

    maybeBlock match {
      case Some(block) => removeBlock(block)
      case None =>
        log.warn(s"Attempted removing block with hash ${ByteStringUtils.hash2string(blockHash)} that we don't have")
    }
  }

  // scalastyle:off method.length
  private def removeBlock(block: Block): Unit = {
    val blockHash = block.hash

    log.debug(s"Trying to remove block ${block.idTag}")

    val txList = block.body.transactionList
    val latestCheckpointNumber = getLatestCheckpointBlockNumber()

    val blockNumberMappingUpdates =
      if (blockchainReader.getHashByBlockNumber(blockchainReader.getBestBranch(), block.number).contains(blockHash))
        removeBlockNumberMapping(block.number)
      else blockNumberMappingStorage.emptyBatchUpdate

    val potentialNewBestBlockNumber: BigInt = (block.number - 1).max(0)
    val potentialNewBestBlockHash: ByteString = block.header.parentHash
    val newLatestCheckpointNumber: BigInt =
      if (block.hasCheckpoint && block.number == latestCheckpointNumber) {
        findPreviousCheckpointBlockNumber(block.number, block.number)
      } else latestCheckpointNumber

    /*
      This two below updates are an exception to the rule of only updating the best blocks when persisting the node
      cache.
      They are required in case we are removing a block that's marked on db as the best (or as the last checkpoint),
      to keep it's consistency, as it will no longer be the best block (nor the last checkpoint).

      This updates can't be done if the conditions are false as we might not have the associated mpt nodes, so falling
      into the case of having an incomplete best block and so an inconsistent db
     */
    val bestBlockNumberUpdates =
      if (appStateStorage.getBestBlockNumber() > potentialNewBestBlockNumber)
        appStateStorage.putBestBlockInfo(BlockInfo(potentialNewBestBlockHash, potentialNewBestBlockNumber))
      else appStateStorage.emptyBatchUpdate
    val latestCheckpointNumberUpdates =
      if (appStateStorage.getLatestCheckpointBlockNumber() > newLatestCheckpointNumber)
        appStateStorage.putLatestCheckpointBlockNumber(newLatestCheckpointNumber)
      else appStateStorage.emptyBatchUpdate

    log.debug(
      "Persisting app info data into database. Persisted block number is {}. Persisted checkpoint number is {}",
      potentialNewBestBlockNumber,
      newLatestCheckpointNumber
    )

    blockHeadersStorage
      .remove(blockHash)
      .and(blockBodiesStorage.remove(blockHash))
      .and(chainWeightStorage.remove(blockHash))
      .and(receiptStorage.remove(blockHash))
      .and(removeTxsLocations(txList))
      .and(blockNumberMappingUpdates)
      .and(bestBlockNumberUpdates)
      .and(latestCheckpointNumberUpdates)
      .commit()

    log.debug(
      "Removed block with hash {}. New best block number - {}, new best checkpoint block number - {}",
      ByteStringUtils.hash2string(blockHash),
      potentialNewBestBlockNumber,
      newLatestCheckpointNumber
    )
  }
  // scalastyle:on method.length

  /** Recursive function which try to find the previous checkpoint by traversing blocks from top to the bottom.
    * In case of finding the checkpoint block number, the function will finish the job and return result
    */
  @tailrec
  private def findPreviousCheckpointBlockNumber(
      blockNumberToCheck: BigInt,
      latestCheckpointBlockNumber: BigInt
  ): BigInt =
    if (blockNumberToCheck > 0) {
      val maybePreviousCheckpointBlockNumber = for {
        currentBlock <- blockchainReader.getBlockByNumber(blockchainReader.getBestBranch(), blockNumberToCheck)
        if currentBlock.hasCheckpoint &&
          currentBlock.number < latestCheckpointBlockNumber
      } yield currentBlock.number

      maybePreviousCheckpointBlockNumber match {
        case Some(previousCheckpointBlockNumber) => previousCheckpointBlockNumber
        case None                                => findPreviousCheckpointBlockNumber(blockNumberToCheck - 1, latestCheckpointBlockNumber)
      }
    } else 0

  private def removeTxsLocations(stxs: Seq[SignedTransaction]): DataSourceBatchUpdate =
    stxs.map(_.hash).foldLeft(transactionMappingStorage.emptyBatchUpdate) { case (updates, hash) =>
      updates.and(transactionMappingStorage.remove(hash))
    }

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy
}

trait BlockchainStorages {
  val blockHeadersStorage: BlockHeadersStorage
  val blockBodiesStorage: BlockBodiesStorage
  val blockNumberMappingStorage: BlockNumberMappingStorage
  val receiptStorage: ReceiptStorage
  val evmCodeStorage: EvmCodeStorage
  val chainWeightStorage: ChainWeightStorage
  val transactionMappingStorage: TransactionMappingStorage
  val appStateStorage: AppStateStorage
  val stateStorage: StateStorage
}

object BlockchainImpl {
  def apply(
      storages: BlockchainStorages,
      blockchainReader: BlockchainReader
  ): BlockchainImpl =
    new BlockchainImpl(
      blockHeadersStorage = storages.blockHeadersStorage,
      blockBodiesStorage = storages.blockBodiesStorage,
      blockNumberMappingStorage = storages.blockNumberMappingStorage,
      receiptStorage = storages.receiptStorage,
      chainWeightStorage = storages.chainWeightStorage,
      transactionMappingStorage = storages.transactionMappingStorage,
      appStateStorage = storages.appStateStorage,
      stateStorage = storages.stateStorage,
      blockchainReader = blockchainReader
    )
}
