package io.iohk.ethereum.domain.branch

import akka.util.ByteString

sealed trait NewBranch

case class BestBranchSubset(tipBlockHash: ByteString, tipBlockNumber: BigInt) extends NewBranch

case object NewEmptyBranch extends NewBranch

/** An interface to manipulate blockchain branches */
trait Branch {

  /** Checks if given block hash is in this chain. (i.e. is an ancestor of the tip block) */
  def isInChain(hash: ByteString): Boolean
}
