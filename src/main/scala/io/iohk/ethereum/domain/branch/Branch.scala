package io.iohk.ethereum.domain.branch

import akka.util.ByteString

import io.iohk.ethereum.domain.Block

sealed trait NewBranch

case class BestBranchSubset(tipBlockHash: ByteString, tipBlockNumber: BigInt) extends NewBranch

case object NewEmptyBranch extends NewBranch

/** An interface to manipulate blockchain branches */
trait Branch {

  /** Returns a block hash for the block at the given height if any */
  def getHashByBlockNumber(number: BigInt): Option[ByteString]

  /** Checks if given block hash is in this chain. (i.e. is an ancestor of the tip block) */
  def isInChain(hash: ByteString): Boolean
}
