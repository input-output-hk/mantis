package io.iohk.ethereum.domain

import java.util.concurrent.atomic.AtomicReference

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.dataSource.RocksDbDataSource.IterationError
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.StateStorage.RollBackFlush
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain
import io.iohk.ethereum.domain.BlockchainImpl.BestBlockLatestCheckpointNumbers
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, MptNode}
import io.iohk.ethereum.vm.{Storage, WorldStateProxy}
import monix.reactive.Observable

import scala.annotation.tailrec

/**
  * Entity to be used to persist and query  Blockchain related objects (blocks, transactions, ommers)
  */
// scalastyle:off number.of.methods
trait Blockchain {

  type S <: Storage[S]
  type WS <: WorldStateProxy[WS, S]

  /**
    * Allows to query a blockHeader by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader]

  def getBlockHeaderByNumber(number: BigInt): Option[BlockHeader] = {
    for {
      hash <- getHashByBlockNumber(number)
      header <- getBlockHeaderByHash(hash)
    } yield header
  }

  /**
    * Allows to query a blockBody by block hash
    *
    * @param hash of the block that's being searched
    * @return [[io.iohk.ethereum.domain.BlockBody]] if found
    */
  def getBlockBodyByHash(hash: ByteString): Option[BlockBody]

  /**
    * Allows to query for a block based on it's hash
    *
    * @param hash of the block that's being searched
    * @return Block if found
    */
  def getBlockByHash(hash: ByteString): Option[Block] =
    for {
      header <- getBlockHeaderByHash(hash)
      body <- getBlockBodyByHash(hash)
    } yield Block(header, body)

  /**
    * Allows to query for a block based on it's number
    *
    * @param number Block number
    * @return Block if it exists
    */
  def getBlockByNumber(number: BigInt): Option[Block] =
    for {
      hash <- getHashByBlockNumber(number)
      block <- getBlockByHash(hash)
    } yield block

  /**
    * Get an account for an address and a block number
    *
    * @param address address of the account
    * @param blockNumber the block that determines the state of the account
    */
  def getAccount(address: Address, blockNumber: BigInt): Option[Account]

  /**
    * Get account storage at given position
    *
    * @param rootHash storage root hash
    * @param position storage position
    */
  def getAccountStorageAt(rootHash: ByteString, position: BigInt, ethCompatibleStorage: Boolean): ByteString

  /**
    * Returns the receipts based on a block hash
    * @param blockhash
    * @return Receipts if found
    */
  def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]]

  /**
    * Returns EVM code searched by it's hash
    * @param hash Code Hash
    * @return EVM code if found
    */
  def getEvmCodeByHash(hash: ByteString): Option[ByteString]

  /**
    * Returns MPT node searched by it's hash
    * @param hash Node Hash
    * @return MPT node
    */
  def getMptNodeByHash(hash: ByteString): Option[MptNode]

  /**
    * Looks up ChainWeight for a given chain
    * @param blockhash Hash of top block in the chain
    * @return ChainWeight if found
    */
  def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight]

  def getChainWeightByNumber(blockNumber: BigInt): Option[ChainWeight] =
    getHashByBlockNumber(blockNumber).flatMap(getChainWeightByHash)

  def getTransactionLocation(txHash: ByteString): Option[TransactionLocation]

  def getBestBlockNumber(): BigInt

  def getBestBlock(): Block

  def getLatestCheckpointBlockNumber(): BigInt

  /**
    * Persists full block along with receipts and chain weight
    * @param saveAsBestBlock - whether to save the block's number as current best block
    */
  def save(block: Block, receipts: Seq[Receipt], chainWeight: ChainWeight, saveAsBestBlock: Boolean): Unit

  /**
    * Persists a block in the underlying Blockchain Database
    * Note: all store* do not update the database immediately, rather they create
    * a [[io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate]] which then has to be committed (atomic operation)
    *
    * @param block Block to be saved
    */
  def storeBlock(block: Block): DataSourceBatchUpdate = {
    storeBlockHeader(block.header).and(storeBlockBody(block.header.hash, block.body))
  }

  def removeBlock(hash: ByteString, withState: Boolean): Unit

  /**
    * Persists a block header in the underlying Blockchain Database
    *
    * @param blockHeader Block to be saved
    */
  def storeBlockHeader(blockHeader: BlockHeader): DataSourceBatchUpdate

  def storeBlockBody(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate

  def storeReceipts(blockHash: ByteString, receipts: Seq[Receipt]): DataSourceBatchUpdate

  def storeEvmCode(hash: ByteString, evmCode: ByteString): DataSourceBatchUpdate

  def storeChainWeight(blockhash: ByteString, weight: ChainWeight): DataSourceBatchUpdate

  def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit

  /**
    * Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  protected def getHashByBlockNumber(number: BigInt): Option[ByteString]

  def genesisHeader: BlockHeader = getBlockHeaderByNumber(0).get

  def genesisBlock: Block = getBlockByNumber(0).get

  def getWorldStateProxy(
      blockNumber: BigInt,
      accountStartNonce: UInt256,
      stateRootHash: Option[ByteString],
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): WS

  def getReadOnlyWorldStateProxy(
      blockNumber: Option[BigInt],
      accountStartNonce: UInt256,
      stateRootHash: Option[ByteString],
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): WS

  def getStateStorage: StateStorage

  def mptStateSavedKeys(): Observable[Either[IterationError, ByteString]]
}
// scalastyle:on

class BlockchainImpl(
    protected val blockHeadersStorage: BlockHeadersStorage,
    protected val blockBodiesStorage: BlockBodiesStorage,
    protected val blockNumberMappingStorage: BlockNumberMappingStorage,
    protected val receiptStorage: ReceiptStorage,
    protected val evmCodeStorage: EvmCodeStorage,
    protected val pruningMode: PruningMode,
    protected val nodeStorage: NodeStorage,
    protected val cachedNodeStorage: CachedNodeStorage,
    protected val chainWeightStorage: ChainWeightStorage,
    protected val transactionMappingStorage: TransactionMappingStorage,
    protected val appStateStorage: AppStateStorage,
    protected val stateStorage: StateStorage
) extends Blockchain {

  override def getStateStorage: StateStorage = stateStorage

  // There is always only one writer thread (ensured by actor), but can by many readers (api calls)
  // to ensure visibility of writes, needs to be volatile or atomic ref
  private val bestKnownBlockAndLatestCheckpoint: AtomicReference[BestBlockLatestCheckpointNumbers] =
    new AtomicReference(BestBlockLatestCheckpointNumbers(BigInt(0), BigInt(0)))

  override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] =
    blockHeadersStorage.get(hash)

  override def getBlockBodyByHash(hash: ByteString): Option[BlockBody] =
    blockBodiesStorage.get(hash)

  override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = receiptStorage.get(blockhash)

  override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = evmCodeStorage.get(hash)

  override def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight] = chainWeightStorage.get(blockhash)

  override def getBestBlockNumber(): BigInt = {
    val bestBlockNum = appStateStorage.getBestBlockNumber()
    if (bestKnownBlockAndLatestCheckpoint.get().bestBlockNumber > bestBlockNum)
      bestKnownBlockAndLatestCheckpoint.get().bestBlockNumber
    else
      bestBlockNum
  }

  override def getLatestCheckpointBlockNumber(): BigInt = {
    val latestCheckpointNumberInStorage = appStateStorage.getLatestCheckpointBlockNumber()
    // The latest checkpoint number is firstly saved in memory and then persisted to the storage only when it's time to persist cache.
    // The latest checkpoint number in memory can be bigger than the number in storage because the cache wasn't persisted yet
    if (bestKnownBlockAndLatestCheckpoint.get().latestCheckpointNumber > latestCheckpointNumberInStorage)
      bestKnownBlockAndLatestCheckpoint.get().latestCheckpointNumber
    else
      latestCheckpointNumberInStorage
  }

  override def getBestBlock(): Block =
    getBlockByNumber(getBestBlockNumber()).get

  override def getAccount(address: Address, blockNumber: BigInt): Option[Account] =
    getBlockHeaderByNumber(blockNumber).flatMap { bh =>
      val storage = stateStorage.getBackingStorage(blockNumber)
      val mpt = MerklePatriciaTrie[Address, Account](
        bh.stateRoot.toArray,
        storage
      )
      mpt.get(address)
    }

  override def getAccountStorageAt(
      rootHash: ByteString,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): ByteString = {
    val storage = stateStorage.getBackingStorage(0)
    val mpt =
      if (ethCompatibleStorage) domain.EthereumUInt256Mpt.storageMpt(rootHash, storage)
      else domain.ArbitraryIntegerMpt.storageMpt(rootHash, storage)
    ByteString(mpt.get(position).getOrElse(BigInt(0)).toByteArray)
  }

  private def persistBestBlocksData(): Unit = {
    appStateStorage
      .putBestBlockNumber(getBestBlockNumber())
      .and(appStateStorage.putLatestCheckpointBlockNumber(getLatestCheckpointBlockNumber()))
      .commit()
  }

  def save(block: Block, receipts: Seq[Receipt], weight: ChainWeight, saveAsBestBlock: Boolean): Unit = {
    storeBlock(block)
      .and(storeReceipts(block.header.hash, receipts))
      .and(storeChainWeight(block.header.hash, weight))
      .commit()

    // not transactional part
    // the best blocks data will be persisted only when the cache will be persisted
    stateStorage.onBlockSave(block.header.number, appStateStorage.getBestBlockNumber())(persistBestBlocksData)

    if (saveAsBestBlock && block.hasCheckpoint) {
      saveBestKnownBlockAndLatestCheckpointNumber(block.header.number, block.header.number)
    } else if (saveAsBestBlock) {
      saveBestKnownBlock(block.header.number)
    }
  }

  override def storeBlockHeader(blockHeader: BlockHeader): DataSourceBatchUpdate = {
    val hash = blockHeader.hash
    blockHeadersStorage.put(hash, blockHeader).and(saveBlockNumberMapping(blockHeader.number, hash))
  }

  override def getMptNodeByHash(hash: ByteString): Option[MptNode] =
    stateStorage.getNode(hash)

  override def getTransactionLocation(txHash: ByteString): Option[TransactionLocation] =
    transactionMappingStorage.get(txHash)

  override def storeBlockBody(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate = {
    blockBodiesStorage.put(blockHash, blockBody).and(saveTxsLocations(blockHash, blockBody))
  }

  override def storeReceipts(blockHash: ByteString, receipts: Seq[Receipt]): DataSourceBatchUpdate =
    receiptStorage.put(blockHash, receipts)

  override def storeEvmCode(hash: ByteString, evmCode: ByteString): DataSourceBatchUpdate =
    evmCodeStorage.put(hash, evmCode)

  override def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit = {
    latestCheckpointNumber match {
      case Some(number) =>
        saveBestKnownBlockAndLatestCheckpointNumber(bestBlockNumber, number)
      case None =>
        saveBestKnownBlock(bestBlockNumber)
    }
  }

  private def saveBestKnownBlock(bestBlockNumber: BigInt): Unit = {
    bestKnownBlockAndLatestCheckpoint.updateAndGet(_.copy(bestBlockNumber = bestBlockNumber))
  }

  private def saveBestKnownBlockAndLatestCheckpointNumber(number: BigInt, latestCheckpointNumber: BigInt): Unit = {
    bestKnownBlockAndLatestCheckpoint.set(BestBlockLatestCheckpointNumbers(number, latestCheckpointNumber))
  }

  def storeChainWeight(blockhash: ByteString, weight: ChainWeight): DataSourceBatchUpdate =
    chainWeightStorage.put(blockhash, weight)

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit = {
    stateStorage.saveNode(nodeHash, nodeEncoded, blockNumber)
  }

  override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)

  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): DataSourceBatchUpdate =
    blockNumberMappingStorage.put(number, hash)

  private def removeBlockNumberMapping(number: BigInt): DataSourceBatchUpdate = {
    blockNumberMappingStorage.remove(number)
  }

// scalastyle:off method.length
  override def removeBlock(blockHash: ByteString, withState: Boolean): Unit = {
    val maybeBlockHeader = getBlockHeaderByHash(blockHash)
    val maybeTxList = getBlockBodyByHash(blockHash).map(_.transactionList)
    val bestBlocks = bestKnownBlockAndLatestCheckpoint.get()
    // as we are decreasing block numbers in memory more often than in storage,
    // we can't use here getBestBlockNumber / getLatestCheckpointBlockNumber
    val bestBlockNumber =
      if (bestBlocks.bestBlockNumber != 0) bestBlocks.bestBlockNumber else appStateStorage.getBestBlockNumber()
    val latestCheckpointNumber = {
      if (bestBlocks.latestCheckpointNumber != 0) bestBlocks.latestCheckpointNumber
      else appStateStorage.getLatestCheckpointBlockNumber()
    }

    val blockNumberMappingUpdates = {
      maybeBlockHeader.fold(blockNumberMappingStorage.emptyBatchUpdate)(h =>
        if (getHashByBlockNumber(h.number).contains(blockHash))
          removeBlockNumberMapping(h.number)
        else blockNumberMappingStorage.emptyBatchUpdate
      )
    }

    val (checkpointUpdates, prevCheckpointNumber): (DataSourceBatchUpdate, Option[BigInt]) = maybeBlockHeader match {
      case Some(header) =>
        if (header.hasCheckpoint && header.number == latestCheckpointNumber) {
          val prev = findPreviousCheckpointBlockNumber(header.number, header.number)
          prev
            .map { num =>
              (appStateStorage.putLatestCheckpointBlockNumber(num), Some(num))
            }
            .getOrElse {
              (appStateStorage.removeLatestCheckpointBlockNumber(), Some(0))
            }
        } else (appStateStorage.emptyBatchUpdate, None)
      case None =>
        (appStateStorage.emptyBatchUpdate, None)
    }

    val newBestBlockNumber: BigInt = if (bestBlockNumber >= 1) bestBlockNumber - 1 else 0

    blockHeadersStorage
      .remove(blockHash)
      .and(blockBodiesStorage.remove(blockHash))
      .and(chainWeightStorage.remove(blockHash))
      .and(receiptStorage.remove(blockHash))
      .and(maybeTxList.fold(transactionMappingStorage.emptyBatchUpdate)(removeTxsLocations))
      .and(blockNumberMappingUpdates)
      .commit()

    // not transactional part
    saveBestKnownBlocks(newBestBlockNumber, prevCheckpointNumber)

    maybeBlockHeader.foreach { h =>
      if (withState) {
        val bestBlocksUpdates = appStateStorage
          .putBestBlockNumber(newBestBlockNumber)
          .and(checkpointUpdates)
        stateStorage.onBlockRollback(h.number, bestBlockNumber)(() => bestBlocksUpdates.commit())
      }
    }
  }
  // scalastyle:on method.length

  def mptStateSavedKeys(): Observable[Either[IterationError, ByteString]] = {
    (nodeStorage.storageContent.map(c => c.map(_._1)) ++ evmCodeStorage.storageContent.map(c => c.map(_._1)))
      .takeWhileInclusive(_.isRight)
  }

  /**
    * Recursive function which try to find the previous checkpoint by traversing blocks from top to the bottom.
    * In case of finding the checkpoint block number, the function will finish the job and return result
    */
  @tailrec
  private def findPreviousCheckpointBlockNumber(
      blockNumberToCheck: BigInt,
      latestCheckpointBlockNumber: BigInt
  ): Option[BigInt] = {
    if (blockNumberToCheck > 0) {
      val maybePreviousCheckpointBlockNumber = for {
        currentBlock <- getBlockByNumber(blockNumberToCheck)
        if currentBlock.hasCheckpoint &&
          currentBlock.number < latestCheckpointBlockNumber
      } yield currentBlock.number

      maybePreviousCheckpointBlockNumber match {
        case Some(_) => maybePreviousCheckpointBlockNumber
        case None => findPreviousCheckpointBlockNumber(blockNumberToCheck - 1, latestCheckpointBlockNumber)
      }
    } else None
  }

  private def saveTxsLocations(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate =
    blockBody.transactionList.zipWithIndex.foldLeft(transactionMappingStorage.emptyBatchUpdate) {
      case (updates, (tx, index)) =>
        updates.and(transactionMappingStorage.put(tx.hash, TransactionLocation(blockHash, index)))
    }

  private def removeTxsLocations(stxs: Seq[SignedTransaction]): DataSourceBatchUpdate = {
    stxs.map(_.hash).foldLeft(transactionMappingStorage.emptyBatchUpdate) { case (updates, hash) =>
      updates.and(transactionMappingStorage.remove(hash))
    }
  }

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy

  override def getWorldStateProxy(
      blockNumber: BigInt,
      accountStartNonce: UInt256,
      stateRootHash: Option[ByteString],
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      stateStorage.getBackingStorage(blockNumber),
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccounts,
      ethCompatibleStorage
    )

  //FIXME Maybe we can use this one in regular execution too and persist underlying storage when block execution is successful
  override def getReadOnlyWorldStateProxy(
      blockNumber: Option[BigInt],
      accountStartNonce: UInt256,
      stateRootHash: Option[ByteString],
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      stateStorage.getReadOnlyStorage,
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccounts = noEmptyAccounts,
      ethCompatibleStorage = ethCompatibleStorage
    )

  //FIXME EC-495 this method should not be need when best block is handled properly during rollback
  def persistCachedNodes(): Unit = {
    if (stateStorage.forcePersist(RollBackFlush)) {
      persistBestBlocksData()
    }
  }
}

trait BlockchainStorages {
  val blockHeadersStorage: BlockHeadersStorage
  val blockBodiesStorage: BlockBodiesStorage
  val blockNumberMappingStorage: BlockNumberMappingStorage
  val receiptStorage: ReceiptStorage
  val evmCodeStorage: EvmCodeStorage
  val chainWeightStorage: ChainWeightStorage
  val transactionMappingStorage: TransactionMappingStorage
  val nodeStorage: NodeStorage
  val pruningMode: PruningMode
  val appStateStorage: AppStateStorage
  val cachedNodeStorage: CachedNodeStorage
  val stateStorage: StateStorage
}

object BlockchainImpl {
  def apply(storages: BlockchainStorages): BlockchainImpl =
    new BlockchainImpl(
      blockHeadersStorage = storages.blockHeadersStorage,
      blockBodiesStorage = storages.blockBodiesStorage,
      blockNumberMappingStorage = storages.blockNumberMappingStorage,
      receiptStorage = storages.receiptStorage,
      evmCodeStorage = storages.evmCodeStorage,
      pruningMode = storages.pruningMode,
      nodeStorage = storages.nodeStorage,
      cachedNodeStorage = storages.cachedNodeStorage,
      chainWeightStorage = storages.chainWeightStorage,
      transactionMappingStorage = storages.transactionMappingStorage,
      appStateStorage = storages.appStateStorage,
      stateStorage = storages.stateStorage
    )

  private case class BestBlockLatestCheckpointNumbers(bestBlockNumber: BigInt, latestCheckpointNumber: BigInt)
}
