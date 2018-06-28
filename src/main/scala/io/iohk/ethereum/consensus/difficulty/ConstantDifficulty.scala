package io.iohk.ethereum.consensus.difficulty

import io.iohk.ethereum.domain.Block

class ConstantDifficulty(c: BigInt) extends DifficultyCalculator {
  final def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parent: Block): BigInt = c
}

