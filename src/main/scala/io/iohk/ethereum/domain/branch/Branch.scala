package io.iohk.ethereum.domain.branch

import akka.util.ByteString

sealed trait NewBranch

case class BestBranchSubset(tipBlockHash: ByteString, tipBlockNumber: BigInt) extends NewBranch

case object NewEmptyBranch extends NewBranch

/** An interface to manipulate blockchain branches */
trait Branch {}
