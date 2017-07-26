package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.{MerklePatriciaTrie, NodesKeyValueStorage}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode
import io.iohk.ethereum.vm.{Storage, UInt256, WorldStateProxy}
import io.iohk.ethereum.network.p2p.messages.PV63.MptNode._

/**
  * Entity to be used to persist and query  Blockchain related objects (blocks, transactions, ommers)
  */
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
    * @return [[io.iohk.ethereum.network.p2p.messages.PV62.BlockBody]] if found
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
  def getAccountStorageAt(rootHash: ByteString, position: BigInt): ByteString

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
    * Returns the total difficulty based on a block hash
    * @param blockhash
    * @return total difficulty if found
    */
  def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt]

  def getTransactionLocation(txHash: ByteString): Option[TransactionLocation]

  /**
    * Persists a block in the underlying Blockchain Database
    *
    * @param block Block to be saved
    */
  def save(block: Block): Unit = {
    save(block.header)
    save(block.header.hash, block.body)
  }

  def removeBlock(hash: ByteString): Unit

  /**
    * Persists a block header in the underlying Blockchain Database
    *
    * @param blockHeader Block to be saved
    */
  def save(blockHeader: BlockHeader): Unit

  def save(blockHash: ByteString, blockBody: BlockBody): Unit

  def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit

  def save(hash: ByteString, evmCode: ByteString): Unit

  def save(blockhash: ByteString, totalDifficulty: BigInt): Unit

  /**
    * Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  protected def getHashByBlockNumber(number: BigInt): Option[ByteString]

  def genesisHeader: BlockHeader = getBlockHeaderByNumber(0).get

  def genesisBlock: Block = getBlockByNumber(0).get

  def getWorldStateProxy(blockNumber: BigInt, accountStartNonce: UInt256, stateRootHash: Option[ByteString] = None): WS

  def getReadOnlyWorldStateProxy(blockNumber: Option[BigInt], accountStartNonce: UInt256, stateRootHash: Option[ByteString] = None): WS
}

class BlockchainImpl(
                      protected val blockHeadersStorage: BlockHeadersStorage,
                      protected val blockBodiesStorage: BlockBodiesStorage,
                      protected val blockNumberMappingStorage: BlockNumberMappingStorage,
                      protected val receiptStorage: ReceiptStorage,
                      protected val evmCodeStorage: EvmCodeStorage,
                      protected val nodesKeyValueStorageFor: Option[BigInt] => NodesKeyValueStorage,
                      protected val totalDifficultyStorage: TotalDifficultyStorage,
                      protected val transactionMappingStorage: TransactionMappingStorage
                    ) extends Blockchain {

  override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] =
    blockHeadersStorage.get(hash)

  override def getBlockBodyByHash(hash: ByteString): Option[BlockBody] =
    blockBodiesStorage.get(hash)

  override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = receiptStorage.get(blockhash)

  override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = evmCodeStorage.get(hash)

  override def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt] = totalDifficultyStorage.get(blockhash)

  override def getAccount(address: Address, blockNumber: BigInt): Option[Account] =
    getBlockHeaderByNumber(blockNumber).flatMap { bh =>
      val mpt = MerklePatriciaTrie[Address, Account](
        bh.stateRoot.toArray,
        nodesKeyValueStorageFor(Some(blockNumber)),
        crypto.kec256(_: Array[Byte])
      )
      mpt.get(address)
    }

  override def getAccountStorageAt(rootHash: ByteString, position: BigInt): ByteString = {
    storageMpt(
      rootHash,
      nodesKeyValueStorageFor(None)
    ).get(UInt256(position)).getOrElse(UInt256(0)).bytes
  }

  override def save(blockHeader: BlockHeader): Unit = {
    val hash = blockHeader.hash
    blockHeadersStorage.put(hash, blockHeader)
    saveBlockNumberMapping(blockHeader.number, hash)
  }

  override def getMptNodeByHash(hash: ByteString): Option[MptNode] = nodesKeyValueStorageFor(None).get(hash).map(_.toMptNode)

  override def getTransactionLocation(txHash: ByteString): Option[TransactionLocation] = transactionMappingStorage.get(txHash)

  override def save(blockHash: ByteString, blockBody: BlockBody): Unit = {
    blockBodiesStorage.put(blockHash, blockBody)
    saveTxsLocations(blockHash, blockBody)
  }

  override def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit = receiptStorage.put(blockHash, receipts)

  override def save(hash: ByteString, evmCode: ByteString): Unit = evmCodeStorage.put(hash, evmCode)

  def save(blockhash: ByteString, td: BigInt): Unit = totalDifficultyStorage.put(blockhash, td)

  override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)

  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): Unit = blockNumberMappingStorage.put(number, hash)

  override def removeBlock(blockHash: ByteString): Unit = {
    val maybeTxList = getBlockBodyByHash(blockHash).map(_.transactionList)
    blockHeadersStorage.remove(blockHash)
    blockBodiesStorage.remove(blockHash)
    totalDifficultyStorage.remove(blockHash)
    receiptStorage.remove(blockHash)
    maybeTxList.foreach(removeTxsLocations)
  }

  private def saveTxsLocations(blockHash: ByteString, blockBody: BlockBody): Unit =
    blockBody.transactionList.zipWithIndex.foreach{ case (tx, index) =>
      transactionMappingStorage.put(tx.hash, TransactionLocation(blockHash, index)) }

  private def removeTxsLocations(stxs: Seq[SignedTransaction]): Unit = {
    stxs.map(_.hash).foreach{ transactionMappingStorage.remove }
  }

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy

  override def getWorldStateProxy(blockNumber: BigInt, accountStartNonce: UInt256, stateRootHash: Option[ByteString]): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      nodesKeyValueStorageFor(Some(blockNumber)),
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash
    )

  //FIXME Maybe we can use this one in regular execution too and persist underlying storage when block execution is successful
  override def getReadOnlyWorldStateProxy(blockNumber: Option[BigInt], accountStartNonce: UInt256, stateRootHash: Option[ByteString]): InMemoryWorldStateProxy =
    InMemoryWorldStateProxy(
      evmCodeStorage,
      ReadOnlyNodeStorage(nodesKeyValueStorageFor(blockNumber)),
      accountStartNonce,
      (number: BigInt) => getBlockHeaderByNumber(number).map(_.hash),
      stateRootHash
    )
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
  val nodesKeyValueStorageFor: (Option[BigInt]) => NodesKeyValueStorage
}

object BlockchainImpl {
  def apply(storages: BlockchainStorages): BlockchainImpl =
    new BlockchainImpl(
      blockHeadersStorage = storages.blockHeadersStorage,
      blockBodiesStorage = storages.blockBodiesStorage,
      blockNumberMappingStorage = storages.blockNumberMappingStorage,
      receiptStorage = storages.receiptStorage,
      evmCodeStorage = storages.evmCodeStorage,
      nodesKeyValueStorageFor = storages.nodesKeyValueStorageFor,
      totalDifficultyStorage = storages.totalDifficultyStorage,
      transactionMappingStorage = storages.transactionMappingStorage
    )
}
