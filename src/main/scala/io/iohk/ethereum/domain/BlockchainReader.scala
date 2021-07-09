package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.BlockBodiesStorage
import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.db.storage.ReceiptStorage
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.branch.{BestBranchSubset, BlockchainBranch, EmptyBranch}
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.utils.Logger

class BlockchainReader(
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    blockNumberMappingStorage: BlockNumberMappingStorage,
    stateStorage: StateStorage,
    receiptStorage: ReceiptStorage,
    appStateStorage: AppStateStorage,
    blockchainMetadata: BlockchainMetadata
) extends Logger {

  /** Allows to query a blockHeader by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] =
    blockHeadersStorage.get(hash)

  /** Allows to query a blockBody by block hash
    *
    * @param hash of the block that's being searched
    * @return [[io.iohk.ethereum.domain.BlockBody]] if found
    */
  def getBlockBodyByHash(hash: ByteString): Option[BlockBody] =
    blockBodiesStorage.get(hash)

  /** Allows to query for a block based on it's hash
    *
    * @param hash of the block that's being searched
    * @return Block if found
    */
  def getBlockByHash(hash: ByteString): Option[Block] =
    for {
      header <- getBlockHeaderByHash(hash)
      body <- getBlockBodyByHash(hash)
    } yield Block(header, body)

  def getBlockHeaderByNumber(branch: BlockchainBranch, number: BigInt): Option[BlockHeader] =
    for {
      hash <- getHashByBlockNumber(branch, number)
      header <- getBlockHeaderByHash(hash)
    } yield header

  /** Returns MPT node searched by it's hash
    * @param hash Node Hash
    * @return MPT node
    */
  def getMptNodeByHash(hash: ByteString): Option[MptNode] =
    stateStorage.getNode(hash)

  /** Returns the receipts based on a block hash
    * @param blockhash
    * @return Receipts if found
    */
  def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = receiptStorage.get(blockhash)

  /** get the current best stored branch */
  def getBestBranch(): BlockchainBranch =
    getBestBlock()
      .map { block =>
        BestBranchSubset(block.header)
      }
      .getOrElse(EmptyBranch)

  def getBestBlockNumber(): BigInt = {
    val bestSavedBlockNumber = appStateStorage.getBestBlockNumber()
    val bestKnownBlockNumber = blockchainMetadata.bestKnownBlockAndLatestCheckpoint.get().bestBlockNumber
    log.debug(
      "Current best saved block number {}. Current best known block number {}",
      bestSavedBlockNumber,
      bestKnownBlockNumber
    )

    // The cached best block number should always be more up-to-date than the one on disk, we are keeping access to disk
    // above only for logging purposes
    bestKnownBlockNumber
  }

  //returns the best known block if it's available in the storage, otherwise the best stored block
  def getBestBlock(): Option[Block] = {
    val bestBlockNumber = getBestBlockNumber()
    log.debug("Trying to get best block with number {}", bestBlockNumber)
    getBlockFromBestChain(bestBlockNumber)
      .orElse(getBlockFromBestChain(bestBlockNumber))
  }

  def genesisHeader: BlockHeader =
    getHeaderFromBestChain(0).get

  def genesisBlock: Block =
    getBlockFromBestChain(0).get

  /** Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  def getHashByBlockNumber(branch: BlockchainBranch, number: BigInt): Option[ByteString] = branch match {
    case EmptyBranch => None
    case BestBranchSubset(tipHeader) =>
      if (tipHeader.number >= number && number >= 0) {
        blockNumberMappingStorage.get(number)
      } else None
  }

  def getBlockByNumber(branch: BlockchainBranch, number: BigInt): Option[Block] = branch match {
    case EmptyBranch => None
    case BestBranchSubset(_) =>
      for {
        hash <- getHashByBlockNumber(branch, number)
        block <- getBlockByHash(hash)
      } yield block
  }

  private def getBlockFromBestChain(number: BigInt): Option[Block] =
    blockNumberMappingStorage
      .get(number)
      .flatMap(getBlockByHash)

  private def getHeaderFromBestChain(number: BigInt): Option[BlockHeader] =
    blockNumberMappingStorage
      .get(number)
      .flatMap(getBlockHeaderByHash)

}

object BlockchainReader {

  def apply(
      storages: BlockchainStorages,
      blockchainMetadata: BlockchainMetadata
  ): BlockchainReader = new BlockchainReader(
    storages.blockHeadersStorage,
    storages.blockBodiesStorage,
    storages.blockNumberMappingStorage,
    storages.stateStorage,
    storages.receiptStorage,
    storages.appStateStorage,
    blockchainMetadata
  )

}
