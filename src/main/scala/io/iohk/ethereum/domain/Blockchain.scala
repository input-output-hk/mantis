package io.iohk.ethereum.domain

import akka.util.ByteString

import cats.instances.option._
import cats.syntax.flatMap._

import scala.annotation.tailrec

import io.iohk.ethereum.db.dataSource.DataSourceBatchUpdate
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain
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

  /** Get an account for an address and a block number
    *
    * @param address address of the account
    * @param blockNumber the block that determines the state of the account
    */
  def getAccount(address: Address, blockNumber: BigInt): Option[Account]

  def getAccountProof(address: Address, blockNumber: BigInt): Option[Vector[MptNode]]

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

  /** Looks up ChainWeight for a given chain
    * @param blockhash Hash of top block in the chain
    * @return ChainWeight if found
    */
  def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight]

  def getLatestCheckpointBlockNumber(): BigInt

  def removeBlock(hash: ByteString, withState: Boolean): Unit

  def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit

  /** Strict check if given block hash is in chain
    * Using any of getXXXByHash is not always accurate - after restart the best block is often lower than before restart
    * The result of that is returning data of blocks which we don't consider as a part of the chain anymore
    * @param hash block hash
    */
  def isInChain(hash: ByteString): Boolean
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
    blockchainReader: BlockchainReader,
    blockchainMetadata: BlockchainMetadata
) extends Blockchain
    with Logger {

  override def isInChain(hash: ByteString): Boolean =
    (for {
      header <- blockchainReader.getBlockHeaderByHash(hash) if header.number <= blockchainReader.getBestBlockNumber()
      bestBranch <- blockchainReader.getBestBranch()
      hash <- bestBranch.getHashByBlockNumber(header.number)
    } yield header.hash == hash).getOrElse(false)

  override def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight] = chainWeightStorage.get(blockhash)

  override def getLatestCheckpointBlockNumber(): BigInt =
    blockchainMetadata.bestKnownBlockAndLatestCheckpoint.get().latestCheckpointNumber

  override def getAccount(address: Address, blockNumber: BigInt): Option[Account] =
    getAccountMpt(blockNumber) >>= (_.get(address))

  override def getAccountProof(address: Address, blockNumber: BigInt): Option[Vector[MptNode]] =
    getAccountMpt(blockNumber) >>= (_.getProof(address))

  private def getAccountMpt(blockNumber: BigInt): Option[MerklePatriciaTrie[Address, Account]] =
    blockchainReader.getBlockHeaderByNumber(blockNumber).map { bh =>
      val storage = stateStorage.getBackingStorage(blockNumber)
      MerklePatriciaTrie[Address, Account](
        rootHash = bh.stateRoot.toArray,
        source = storage
      )
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

  private def persistBestBlocksData(): Unit = {
    val currentBestBlockNumber = blockchainReader.getBestBlockNumber()
    val currentBestCheckpointNumber = getLatestCheckpointBlockNumber()
    log.debug(
      "Persisting app info data into database. Persisted block number is {}. " +
        "Persisted checkpoint number is {}",
      currentBestBlockNumber,
      currentBestCheckpointNumber
    )

    appStateStorage
      .putBestBlockNumber(currentBestBlockNumber)
      .and(appStateStorage.putLatestCheckpointBlockNumber(currentBestCheckpointNumber))
      .commit()
  }

  override def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit =
    latestCheckpointNumber match {
      case Some(number) =>
        saveBestKnownBlockAndLatestCheckpointNumber(bestBlockNumber, number)
      case None =>
        saveBestKnownBlock(bestBlockNumber)
    }

  private def saveBestKnownBlock(bestBlockNumber: BigInt): Unit =
    blockchainMetadata.bestKnownBlockAndLatestCheckpoint.updateAndGet(_.copy(bestBlockNumber = bestBlockNumber))

  private def saveBestKnownBlockAndLatestCheckpointNumber(number: BigInt, latestCheckpointNumber: BigInt): Unit =
    blockchainMetadata.bestKnownBlockAndLatestCheckpoint.set(
      BestBlockLatestCheckpointNumbers(number, latestCheckpointNumber)
    )

  private def removeBlockNumberMapping(number: BigInt): DataSourceBatchUpdate =
    blockNumberMappingStorage.remove(number)

  override def removeBlock(blockHash: ByteString, withState: Boolean): Unit = {
    val maybeBlock = blockchainReader.getBlockByHash(blockHash)

    maybeBlock match {
      case Some(block) => removeBlock(block, withState)
      case None =>
        log.warn(s"Attempted removing block with hash ${ByteStringUtils.hash2string(blockHash)} that we don't have")
    }
  }

  // scalastyle:off method.length
  private def removeBlock(block: Block, withState: Boolean): Unit = {
    val blockHash = block.hash

    log.debug(s"Trying to remove block ${block.idTag}")

    val txList = block.body.transactionList
    val bestBlockNumber = blockchainReader.getBestBlockNumber()
    val latestCheckpointNumber = getLatestCheckpointBlockNumber()

    val blockNumberMappingUpdates =
      if (blockchainReader.getBestBranch().flatMap(_.getHashByBlockNumber(block.number)).contains(blockHash))
        removeBlockNumberMapping(block.number)
      else blockNumberMappingStorage.emptyBatchUpdate

    val newBestBlockNumber: BigInt = (bestBlockNumber - 1).max(0)
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
      if (appStateStorage.getBestBlockNumber() > newBestBlockNumber)
        appStateStorage.putBestBlockNumber(newBestBlockNumber)
      else appStateStorage.emptyBatchUpdate
    val latestCheckpointNumberUpdates =
      if (appStateStorage.getLatestCheckpointBlockNumber() > newLatestCheckpointNumber)
        appStateStorage.putLatestCheckpointBlockNumber(newLatestCheckpointNumber)
      else appStateStorage.emptyBatchUpdate

    log.debug(
      "Persisting app info data into database. Persisted block number is {}. Persisted checkpoint number is {}",
      newBestBlockNumber,
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

    saveBestKnownBlocks(newBestBlockNumber, Some(newLatestCheckpointNumber))
    log.debug(
      "Removed block with hash {}. New best block number - {}, new best checkpoint block number - {}",
      ByteStringUtils.hash2string(blockHash),
      newBestBlockNumber,
      newLatestCheckpointNumber
    )

    // not transactional part
    if (withState)
      stateStorage.onBlockRollback(block.number, bestBlockNumber)(() => persistBestBlocksData())
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
        currentBlock <- blockchainReader.getBestBranch().flatMap(_.getBlockByNumber(blockNumberToCheck))
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
      blockchainReader: BlockchainReader,
      metadata: BlockchainMetadata
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
      blockchainReader = blockchainReader,
      blockchainMetadata = metadata
    )
}
