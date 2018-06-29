package io.iohk.ethereum.consensus.difficulty

import io.iohk.ethereum.domain.Block

trait DifficultyCalculator {
  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parent: Block): BigInt
}
