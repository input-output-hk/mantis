package io.iohk.ethereum.domain

import akka.util.ByteString
import io.iohk.ethereum.db.storage.{BlockBodiesStorage, BlockHeadersStorage, BlockNumberMappingStorage}

class BlockchainReader(
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    blockNumberMappingStorage: BlockNumberMappingStorage
) {

  /**
    * Allows to query a blockHeader by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] =
    blockHeadersStorage.get(hash)

  /**
    * Allows to query a blockBody by block hash
    *
    * @param hash of the block that's being searched
    * @return [[io.iohk.ethereum.domain.BlockBody]] if found
    */
  def getBlockBodyByHash(hash: ByteString): Option[BlockBody] =
    blockBodiesStorage.get(hash)

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
    * Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  def getHashByBlockNumber(number: BigInt): Option[ByteString] =
    blockNumberMappingStorage.get(number)

  def getBlockHeaderByNumber(number: BigInt): Option[BlockHeader] = {
    for {
      hash <- getHashByBlockNumber(number)
      header <- getBlockHeaderByHash(hash)
    } yield header
  }

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
}
