package io.iohk.ethereum.consensus.difficulty

import io.iohk.ethereum.domain.BlockHeader

trait DifficultyCalculator {
  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt
}
