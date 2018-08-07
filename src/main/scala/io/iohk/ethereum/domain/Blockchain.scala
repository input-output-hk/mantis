package io.iohk.ethereum.domain

import java.util.concurrent.atomic.AtomicReference
import akka.util.ByteString
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, BasicPruning, FastSyncPruning, PruningMode}
import io.iohk.ethereum.domain
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, MptNode, NodesKeyValueStorage}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV63.MptNodeEncoders._
import io.iohk.ethereum.vm.{Storage, WorldStateProxy}

/** Entity to be used to persist and query [[Blockchain]] related objects (blocks, transactions, ommers) */
// scalastyle:off number.of.methods
trait Blockchain {

  type S <: Storage[S]
  type WS <: WorldStateProxy[WS, S]

  /** Allows to query a blockHeader by block hash
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

  /** Allows to query a blockBody by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockBody]] if found
    */
  def getBlockBodyByHash(hash: ByteString): Option[BlockBody]

  /** Allows to query for a block based on it's hash
    *
    * @param hash of the block that's being searched
    * @return [[Block]] if found
    */
  def getBlockByHash(hash: ByteString): Option[Block] =
    for {
      header <- getBlockHeaderByHash(hash)
      body <- getBlockBodyByHash(hash)
    } yield Block(header, body)

  /** Allows to query for a block based on it's number
    *
    * @param number Block number
    * @return [[Block]] if it exists
    */
  def getBlockByNumber(number: BigInt): Option[Block] =
    for {
      hash <- getHashByBlockNumber(number)
      block <- getBlockByHash(hash)
    } yield block

  /** Get an account by an address and a block number
    *
    * @param address address of the account
    * @param blockNumber the block that determines the state of the account
    */
  def getAccount(address: Address, blockNumber: BigInt): Option[Account]

  /** Get account storage at given position
    *
    * @param rootHash storage root hash
    * @param position storage position
    */
  def getAccountStorageAt(rootHash: ByteString, position: BigInt, ethCompatibleStorage: Boolean): ByteString

  /** Returns the receipts based on a block hash
    *
    * @param blockhash the hash of the block that's being searched for receipts
    * @return Receipts if found
    */
  def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]]

  /** Returns EVM code searched by it's hash
    *
    * @param hash Code Hash
    * @return EVM code if found
    */
  def getEvmCodeByHash(hash: ByteString): Option[ByteString]

  /** Returns MPT node searched by it's hash
    *
    * @param hash Node Hash
    * @return MPT node
    */
  def getMptNodeByHash(hash: ByteString): Option[MptNode]

  /** Returns the total difficulty based on a block hash
    *
    * @param blockhash the hash of the block that's being searched for totalDifficulty
    * @return total difficulty if found
    */
  def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt]

  def getTotalDifficultyByNumber(blockNumber: BigInt): Option[BigInt] =
    getHashByBlockNumber(blockNumber).flatMap(getTotalDifficultyByHash)

  def getTransactionLocation(txHash: ByteString): Option[TransactionLocation]

  def getBestBlockNumber(): BigInt

  def getBestBlock(): Block

  /** Persists full block along with receipts and total difficulty
    *
    * @param saveAsBestBlock whether to save the block's number as current best block
    */
  def save(block: Block, receipts: Seq[Receipt], totalDifficulty: BigInt, saveAsBestBlock: Boolean): Unit = {
    save(block)
    save(block.header.hash, receipts)
    save(block.header.hash, totalDifficulty)
    pruneState(block.header.number)
    checkAndPersistCachedNodes()
    if (saveAsBestBlock) {
      saveBestKnownBlock(block.header.number)
    }
  }

  /** Persists a block in the underlying Blockchain Database
    *
    * @param block Block to be saved
    */
  def save(block: Block): Unit = {
    save(block.header)
    save(block.header.hash, block.body)
  }

  def removeBlock(hash: ByteString, withState: Boolean): Unit

  /** Persists a block header in the underlying Blockchain Database
    *
    * @param blockHeader Block to be saved
    */
  def save(blockHeader: BlockHeader): Unit

  def save(blockHash: ByteString, blockBody: BlockBody): Unit

  def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit

  def save(hash: ByteString, evmCode: ByteString): Unit

  def save(blockhash: ByteString, totalDifficulty: BigInt): Unit

  def saveBestKnownBlock(number: BigInt): Unit

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit

  def saveFastSyncNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit

  /** Returns a block hash given a block number
    *
    * @param number Number of the searched block
    * @return Block hash if found
    */
  protected def getHashByBlockNumber(number: BigInt): Option[ByteString]

  def genesisHeader: BlockHeader = getBlockHeaderByNumber(0).get

  def genesisBlock: Block = getBlockByNumber(0).get

  def getWorldStateProxy(blockNumber: BigInt,
                         accountStartNonce: UInt256,
                         stateRootHash: Option[ByteString],
                         noEmptyAccounts: Boolean,
                         ethCompatibleStorage: Boolean): WS

  def getReadOnlyWorldStateProxy(blockNumber: Option[BigInt],
                                 accountStartNonce: UInt256,
                                 stateRootHash: Option[ByteString],
                                 noEmptyAccounts: Boolean,
                                 ethCompatibleStorage: Boolean): WS

  def pruneState(blockNumber: BigInt): Unit

  def rollbackStateChangesMadeByBlock(blockNumber: BigInt): Unit

  def checkAndPersistCachedNodes(): Unit
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
    protected val totalDifficultyStorage: TotalDifficultyStorage,
    protected val transactionMappingStorage: TransactionMappingStorage,
    protected val appStateStorage: AppStateStorage
) extends Blockchain {

  // There is always only one writer thread (ensured by actor), but can by many readers (api calls)
  // to ensure visibility of writes, needs to be volatile or atomic ref
  private val bestKnownBlock: AtomicReference[BigInt] = new AtomicReference(BigInt(0))

  override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] =
    blockHeadersStorage.get(hash)

  override def getBlockBodyByHash(hash: ByteString): Option[BlockBody] =
    blockBodiesStorage.get(hash)

  override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = receiptStorage.get(blockhash)

  override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = evmCodeStorage.get(hash)

  override def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt] = totalDifficultyStorage.get(blockhash)

  override def getBestBlockNumber(): BigInt = {
    val bestBlockNum = appStateStorage.getBestBlockNumber()
    if (bestKnownBlock.get() > bestBlockNum)
      bestKnownBlock.get()
    else
      bestBlockNum
  }

  override def getBestBlock(): Block =
    getBlockByNumber(getBestBlockNumber()).get

  override def getAccount(address: Address, blockNumber: BigInt): Option[Account] =
    getBlockHeaderByNumber(blockNumber).flatMap { bh =>
      val mpt = MerklePatriciaTrie[Address, Account](
        bh.stateRoot.toArray,
        nodesKeyValueStorageFor(Some(blockNumber), cachedNodeStorage)
      )
      mpt.get(address)
    }

  override def getAccountStorageAt(rootHash: ByteString, position: BigInt, ethCompatibleStorage: Boolean): ByteString = {
    val mpt =
      if (ethCompatibleStorage) domain.EthereumUInt256Mpt.storageMpt(rootHash, nodesKeyValueStorageFor(None, cachedNodeStorage))
      else domain.ArbitraryIntegerMpt.storageMpt(rootHash, nodesKeyValueStorageFor(None, cachedNodeStorage))
    ByteString(mpt.get(position).getOrElse(BigInt(0)).toByteArray)
  }

  override def save(blockHeader: BlockHeader): Unit = {
    val hash = blockHeader.hash
    blockHeadersStorage.put(hash, blockHeader)
    saveBlockNumberMapping(blockHeader.number, hash)
  }

  override def getMptNodeByHash(hash: ByteString): Option[MptNode] =
    nodesKeyValueStorageFor(None, cachedNodeStorage).get(hash).map(_.toMptNode)

  override def getTransactionLocation(txHash: ByteString): Option[TransactionLocation] = transactionMappingStorage.get(txHash)

  override def save(blockHash: ByteString, blockBody: BlockBody): Unit = {
    blockBodiesStorage.put(blockHash, blockBody)
    saveTxsLocations(blockHash, blockBody)
  }

  override def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit = receiptStorage.put(blockHash, receipts)

  override def save(hash: ByteString, evmCode: ByteString): Unit = evmCodeStorage.put(hash, evmCode)

  override def saveBestKnownBlock(number: BigInt): Unit = {
    bestKnownBlock.set(number)
  }

  def save(blockhash: ByteString, td: BigInt): Unit = totalDifficultyStorage.put(blockhash, td)

  def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit = {
    nodesKeyValueStorageFor(Some(blockNumber), nodeStorage).put(nodeHash, nodeEncoded)
  }

  // During fastSync or genesis data loading, we can omit reference counting when pruning is turned on. It is possible
  // because there exists only one block (one mpt trie) so every saved node will have reference count equal to 1.
  def saveFastSyncNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit = {
    import io.iohk.ethereum.db.storage.pruning
    pruningMode match {
      case ArchivePruning => saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt)
      case _ => PruningMode.nodesKeyValueStorage(pruning.FastSyncPruning, nodeStorage)(Some(blockNumber)).put(nodeHash, nodeEncoded)
    }
  }

  override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)

  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): Unit =
    blockNumberMappingStorage.put(number, hash)

  private def removeBlockNumberMapping(number: BigInt): Unit = {
    blockNumberMappingStorage.remove(number)
    appStateStorage.putBestBlockNumber(number - 1) // FIXME: mother of consistency?!?!
  }

  override def removeBlock(blockHash: ByteString, withState: Boolean): Unit = {
    val maybeBlockHeader = getBlockHeaderByHash(blockHash)
    val maybeTxList = getBlockBodyByHash(blockHash).map(_.transactionList)

    blockHeadersStorage.remove(blockHash)
    blockBodiesStorage.remove(blockHash)
    totalDifficultyStorage.remove(blockHash)
    receiptStorage.remove(blockHash)
    maybeTxList.foreach(removeTxsLocations)
    maybeBlockHeader.foreach{ h =>
      if (withState)
        rollbackStateChangesMadeByBlock(h.number)

      if (getHashByBlockNumber(h.number).contains(blockHash))
        removeBlockNumberMapping(h.number)
    }

    if (withState)
      checkAndPersistCachedNodes()
  }

  private def saveTxsLocations(blockHash: ByteString, blockBody: BlockBody): Unit =
    blockBody.transactionList.zipWithIndex.foreach{ case (tx, index) =>
      transactionMappingStorage.put(tx.hash, TransactionLocation(blockHash, index)) }

  private def removeTxsLocations(stxs: Seq[SignedTransaction]): Unit = {
    stxs.map(_.hash).foreach{ transactionMappingStorage.remove }
  }

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy

  override def getWorldStateProxy(blockNumber: BigInt,
                                  accountStartNonce: UInt256,
                                  stateRootHash: Option[ByteString],
                                  noEmptyAccounts: Boolean,
                                  ethCompatibleStorage: Boolean): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      nodesKeyValueStorageFor(Some(blockNumber), cachedNodeStorage),
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccounts,
      ethCompatibleStorage
    )

  // FIXME Maybe we can use this one in regular execution too
  // and persist underlying storage when block execution is successful
  override def getReadOnlyWorldStateProxy(blockNumber: Option[BigInt],
                                          accountStartNonce: UInt256,
                                          stateRootHash: Option[ByteString],
                                          noEmptyAccounts: Boolean,
                                          ethCompatibleStorage: Boolean): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      ReadOnlyNodeStorage(nodesKeyValueStorageFor(blockNumber, cachedNodeStorage)),
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash,
      noEmptyAccounts = false,
      ethCompatibleStorage = ethCompatibleStorage
    )

  def nodesKeyValueStorageFor(blockNumber: Option[BigInt], storage: NodesStorage): NodesKeyValueStorage =
    PruningMode.nodesKeyValueStorage(pruningMode, storage)(blockNumber)

  def pruneState(blockNumber: BigInt): Unit = {
    val blockToPrune = getBlockToPrune(blockNumber)
    val currentBestSavedBlock = appStateStorage.getBestBlockNumber()

    if (blockToPrune <= currentBestSavedBlock){
      PruningMode.prune(pruningMode, blockNumber, cachedNodeStorage, inMemory = false)
    } else{
      PruningMode.prune(pruningMode, blockNumber, cachedNodeStorage, inMemory = true)
    }
  }

  def rollbackStateChangesMadeByBlock(blockNumber: BigInt): Unit ={
    val currentBestSavedBlock = appStateStorage.getBestBlockNumber()

    if (blockNumber <= currentBestSavedBlock)
      PruningMode.rollback(pruningMode, blockNumber, cachedNodeStorage, inMemory = false)
    else
      PruningMode.rollback(pruningMode, blockNumber, cachedNodeStorage, inMemory = true)
  }

  // FIXME EC-495 this method should not be need when best block is handled properly during rollback
  def persistCachedNodes(): Unit = {
    cachedNodeStorage.forcePersist()
    appStateStorage.putBestBlockNumber(getBestBlockNumber())
  }

  def checkAndPersistCachedNodes(): Unit = {
    if (cachedNodeStorage.persist()){
      appStateStorage.putBestBlockNumber(getBestBlockNumber())
    }
  }

  private def getBlockToPrune(blockNumber: BigInt): BigInt = {
    pruningMode match {
      case ArchivePruning => 0
      case BasicPruning(history) => blockNumber - history
      case FastSyncPruning => 0
    }
  }
}

trait BlockchainStorages {
  val blockHeadersStorage: BlockHeadersStorage
  val blockBodiesStorage: BlockBodiesStorage
  val blockNumberMappingStorage: BlockNumberMappingStorage
  val receiptStorage: ReceiptStorage
  val evmCodeStorage: EvmCodeStorage
  val totalDifficultyStorage: TotalDifficultyStorage
  val transactionMappingStorage: TransactionMappingStorage
  val nodeStorage: NodeStorage
  val pruningMode: PruningMode
  val appStateStorage: AppStateStorage
  val cachedNodeStorage: CachedNodeStorage
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
      totalDifficultyStorage = storages.totalDifficultyStorage,
      transactionMappingStorage = storages.transactionMappingStorage,
      appStateStorage = storages.appStateStorage
    )
}
