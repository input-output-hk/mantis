package io.iohk.ethereum.domain

import akka.util.ByteString

import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.BlockBodiesStorage
import io.iohk.ethereum.db.storage.BlockHeadersStorage
import io.iohk.ethereum.db.storage.BlockNumberMappingStorage
import io.iohk.ethereum.db.storage.ChainWeightStorage
import io.iohk.ethereum.db.storage.ReceiptStorage
import io.iohk.ethereum.db.storage.StateStorage
import io.iohk.ethereum.domain.branch.BestBranch
import io.iohk.ethereum.domain.branch.Branch
import io.iohk.ethereum.domain.branch.EmptyBranch
import io.iohk.ethereum.ledger.BlockData
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.utils.Hex
import io.iohk.ethereum.utils.Logger

class BlockchainReader(
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    blockNumberMappingStorage: BlockNumberMappingStorage,
    stateStorage: StateStorage,
    receiptStorage: ReceiptStorage,
    appStateStorage: AppStateStorage,
    chainWeightStorage: ChainWeightStorage
) extends Logger {

  /** Assemble the BlockData that should be available after a block gets executed
    * @param block
    * @return
    */
  def getBlockData(block: Block): BlockData =
    BlockData(
      block,
      receiptStorage.get(block.hash).getOrElse(Seq.empty),
      chainWeightStorage.get(block.hash).getOrElse(ChainWeight.zero)
    )

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
  def getBestBranch(): Branch = {
    val number = getBestBlockNumber()
    blockNumberMappingStorage
      .get(number)
      .orElse(blockNumberMappingStorage.get(appStateStorage.getBestBlockNumber()))
      .map(hash => BestBranch(hash, number))
      .getOrElse(EmptyBranch)
  }

  def getBestBlockNumber(): BigInt = appStateStorage.getBestBlockNumber()

  def getLatestCheckpointBlockNumber(): BigInt = appStateStorage.getLatestCheckpointBlockNumber()

  //returns the best known block if it's available in the storage
  def getBestBlock(): Option[Block] = {
    val bestKnownBlockinfo = appStateStorage.getBestBlockInfo()
    log.debug("Trying to get best block with number {}", bestKnownBlockinfo.number)
    val bestBlock = getBlockByHash(bestKnownBlockinfo.hash)
    if (bestBlock.isEmpty) {
      log.error(
        "Best block {} (number: {}) not found in storage.",
        Hex.toHexString(bestKnownBlockinfo.hash.toArray),
        bestKnownBlockinfo.number
      )
    }
    bestBlock
  }

  def genesisHeader: BlockHeader =
    getBlockHeaderByNumber(0).get

  def genesisBlock: Block =
    getBlockByNumber(0).get

  /** Returns a block inside this branch based on its number */
  def getBlockByNumber(branch: Branch, number: BigInt): Option[Block] = branch match {
    case BestBranch(_, tipBlockNumber) if tipBlockNumber >= number && number >= 0 =>
      for {
        hash <- getHashByBlockNumber(number)
        block <- getBlockByHash(hash)
      } yield block
    case EmptyBranch | BestBranch(_, _) => None
  }

  /** Returns a block hash for the block at the given height if any */
  def getHashByBlockNumber(branch: Branch, number: BigInt): Option[ByteString] = branch match {
    case BestBranch(_, tipBlockNumber) =>
      if (tipBlockNumber >= number && number >= 0) {
        blockNumberMappingStorage.get(number)
      } else None

    case EmptyBranch => None
  }

  /** Checks if given block hash is in this chain. (i.e. is an ancestor of the tip block) */
  def isInChain(branch: Branch, hash: ByteString): Boolean = branch match {
    case BestBranch(_, tipBlockNumber) =>
      (for {
        header <- getBlockHeaderByHash(hash) if header.number <= tipBlockNumber
        hashFromBestChain <- getHashByBlockNumber(branch, header.number)
      } yield header.hash == hashFromBestChain).getOrElse(false)
    case EmptyBranch => false
  }

  /** Get an account for an address and a block number
    *
    * @param branch branch for which we want to get the account
    * @param address address of the account
    * @param blockNumber the block that determines the state of the account
    */
  def getAccount(branch: Branch, address: Address, blockNumber: BigInt): Option[Account] = branch match {
    case BestBranch(_, tipBlockNumber) =>
      if (blockNumber <= tipBlockNumber)
        getAccountMpt(blockNumber).flatMap(_.get(address))
      else
        None
    case EmptyBranch => None
  }

  def getAccountProof(branch: Branch, address: Address, blockNumber: BigInt): Option[Vector[MptNode]] =
    branch match {
      case BestBranch(_, tipBlockNumber) =>
        if (blockNumber <= tipBlockNumber)
          getAccountMpt(blockNumber).flatMap(_.getProof(address))
        else
          None
      case EmptyBranch => None
    }

  /** Looks up ChainWeight for a given chain
    * @param blockhash Hash of top block in the chain
    * @return ChainWeight if found
    */
  def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight] = chainWeightStorage.get(blockhash)

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
    * @param number Number of the searched block
    * @return Block hash if found
    */
  private def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)

  private def getAccountMpt(blockNumber: BigInt): Option[MerklePatriciaTrie[Address, Account]] =
    getBlockHeaderByNumber(blockNumber).map { bh =>
      val storage = stateStorage.getBackingStorage(blockNumber)
      MerklePatriciaTrie[Address, Account](
        rootHash = bh.stateRoot.toArray,
        source = storage
      )
    }
}

object BlockchainReader {

  def apply(
      storages: BlockchainStorages
  ): BlockchainReader = new BlockchainReader(
    storages.blockHeadersStorage,
    storages.blockBodiesStorage,
    storages.blockNumberMappingStorage,
    storages.stateStorage,
    storages.receiptStorage,
    storages.appStateStorage,
    storages.chainWeightStorage
  )

}
