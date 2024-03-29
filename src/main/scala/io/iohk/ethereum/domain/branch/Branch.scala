package io.iohk.ethereum.domain.branch

import akka.util.ByteString

sealed trait Branch

case class BestBranch(tipBlockHash: ByteString, tipBlockNumber: BigInt) extends Branch

case object EmptyBranch extends Branch
