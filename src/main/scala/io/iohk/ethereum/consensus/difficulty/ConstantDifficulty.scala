package io.iohk.ethereum.consensus.difficulty

import io.iohk.ethereum.domain.BlockHeader

class ConstantDifficulty(c: BigInt) extends DifficultyCalculator {
  final def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt = c
}

