package io.iohk.ethereum.domain.branch

import akka.util.ByteString

import io.iohk.ethereum.domain.Block

// TODO choose a name : ChainInstance, BlockchainBranch, Branch, Blockchain ?
trait BlockchainBranch {

  /** Returns a block inside this branch based on its number */
  def getBlockByNumber(number: BigInt): Option[Block]

  /** Returns a block hash for the block at the given height if any */
  def getHashByBlockNumber(number: BigInt): Option[ByteString]

}
