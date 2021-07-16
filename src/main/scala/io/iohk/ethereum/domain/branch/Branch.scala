package io.iohk.ethereum.domain.branch

import akka.util.ByteString

import io.iohk.ethereum.domain.Block

/** An interface to manipulate blockchain branches */
trait Branch {

  /** Returns a block inside this branch based on its number */
  def getBlockByNumber(number: BigInt): Option[Block]

  /** Returns a block hash for the block at the given height if any */
  def getHashByBlockNumber(number: BigInt): Option[ByteString]

}
