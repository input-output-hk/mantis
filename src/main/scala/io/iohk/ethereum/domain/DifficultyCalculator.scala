package io.iohk.ethereum.domain

import io.iohk.ethereum.utils.BlockchainConfig

class DifficultyCalculator(blockchainConfig: BlockchainConfig) {
  val DifficultyBoundDivision: Int = 2048
  val FrontierTimestampDiffLimit: Int = -99
  val ExpDifficultyPeriod: Int = 100000
  val MinimumDifficulty: BigInt = 131072

  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt = {
    import blockchainConfig.{homesteadBlockNumber, difficultyBombPauseBlockNumber, difficultyBombContinueBlockNumber}

    val x: BigInt = parentHeader.difficulty / DifficultyBoundDivision
    val c: BigInt =
      if (blockNumber < homesteadBlockNumber) {
        if (blockTimestamp < parentHeader.unixTimestamp + 13) 1 else -1
      } else {
        val timestampDiff = blockTimestamp - parentHeader.unixTimestamp
        math.max(1 - timestampDiff / 10, FrontierTimestampDiffLimit)
      }

    val difficultyBombExponent: Int =
      if (blockNumber < difficultyBombPauseBlockNumber)
        (blockNumber / ExpDifficultyPeriod - 2).toInt
      else if (blockNumber < difficultyBombContinueBlockNumber)
        ((difficultyBombPauseBlockNumber / ExpDifficultyPeriod) - 2).toInt
      else {
        val delay = (difficultyBombContinueBlockNumber - difficultyBombPauseBlockNumber) / ExpDifficultyPeriod
        ((blockNumber / ExpDifficultyPeriod) - delay - 2).toInt
      }

    val difficultyBomb: BigInt =
      if(difficultyBombExponent >= 0)
        BigInt(2).pow(difficultyBombExponent)
      else 0

    val difficultyWithoutBomb = MinimumDifficulty.max(parentHeader.difficulty + x * c)
    difficultyWithoutBomb + difficultyBomb
  }
}
