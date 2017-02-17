package io.iohk.ethereum.blockchain

import akka.util.ByteString
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage, BlockNumberMappingStorage}
import io.iohk.ethereum.domain.{Block, BlockHeader}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody

/**
  * This interface presents an API to persist and query blocks within the blockchain.
  */
trait BlocksRepository {
  protected def blockHeadersStorage: BlockHeadersStorage
  protected def blockBodiesStorage: BlockBodiesStorage
  protected def blockNumberMappingStorage: BlockNumberMappingStorage

  /**
    * Allows to query a blockHeader by block hash
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] = blockHeadersStorage.get(hash)

  /**
    * Allows to query a blockBody by block hash
    * @param hash of the block that's being searched
    * @return [[BlockBody]] if found
    */
  def getBlockBodyByHash(hash: ByteString): Option[BlockBody] = blockBodiesStorage.get(hash)

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
    * @param number
    * @return
    */
  def getBlockByNumber(number: BigInt): Option[Block] =
    for {
      hash <- getHashByBlockNumber(number)
      block <- getBlockByHash(hash)
    } yield block

  /**
    * Persists a block in the underlaying Blockchain Database
    *
    * @param block Block to be saved
    */
  def save(block: Block): Unit = {
    val hash = block.header.hash
    blockHeadersStorage.put(hash, block.header)
    blockBodiesStorage.put(hash, block.body)
    saveBlockNumberMapping(block.header.number, hash)
  }

  /**
    * Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  //FIXME This method doesn't deal with having multiple forked chains at the same time yet
  private def getHashByBlockNumber(number: BigInt): Option[ByteString] = blockNumberMappingStorage.get(number)

  /**
    * Saves the number -> hash relationship in order to be used within queries
    *
    * @param number Block Number to be indexed
    * @param hash   Related hash for that given block
    */
  //FIXME This method doesn't deal with having multiple forked chains at the same time yet
  private def saveBlockNumberMapping(number: BigInt, hash: ByteString): Unit = blockNumberMappingStorage.put(number, hash)
}
