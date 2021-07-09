package io.iohk.ethereum.domain.branch

import io.iohk.ethereum.domain.{Block, BlockHeader}

sealed trait BlockchainBranch

case class BestBranchSubset(tipHeader: BlockHeader) extends BlockchainBranch

case object EmptyBranch extends BlockchainBranch
