package io.iohk.ethereum.domain

import akka.util.ByteString

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.BlockBodiesStorage
import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.db.storage.ReceiptStorage
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.branch.BestBlockchainBranch
import io.iohk.ethereum.domain.branch.BlockchainBranch
import io.iohk.ethereum.domain.branch.EmptyBlockchainBranch
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

  def getBlockHeaderByNumber(number: BigInt): Option[BlockHeader] =
    for {
      hash <- getHashByBlockNumber(number)
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
        new BestBlockchainBranch(
          block.header,
          blockNumberMappingStorage,
          this
        )
      }
      .getOrElse(EmptyBlockchainBranch)

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
    getBlockByNumber(bestBlockNumber).orElse(
      getBlockByNumber(
        appStateStorage.getBestBlockNumber()
      )
    )
  }

  def genesisHeader: BlockHeader =
    getBlockHeaderByNumber(0).get

  def genesisBlock: Block =
    getBlockByNumber(0).get

  /** Allows to query for a block based on it's number
    *
    * @param number Block number
    * @return Block if it exists
    */
  private def getBlockByNumber(number: BigInt): Option[Block] =
    for {
      hash <- getHashByBlockNumber(number)
      block <- getBlockByHash(hash)
    } yield block

  /** Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  private def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)
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
