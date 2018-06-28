package io.iohk.ethereum.consensus.ethash.difficulty

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class EthashDifficultyCalculator(blockchainConfig: BlockchainConfig) extends DifficultyCalculator {
  import blockchainConfig.{
    difficultyBombContinueBlockNumber,
    difficultyBombPauseBlockNumber,
    homesteadBlockNumber,
    difficultyBombRemovalBlockNumber,
    byzantiumBlockNumber
  }

  val DifficultyBoundDivision: Int = 2048
  val FrontierTimestampDiffLimit: Int = -99
  val ExpDifficultyPeriod: Int = 100000
  val MinimumDifficulty: BigInt = 131072
  val RelaxDifficulty: BigInt = 3000000

  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt = {
    val x: BigInt = parentHeader.difficulty / DifficultyBoundDivision
    val c: BigInt =
      if (blockNumber < homesteadBlockNumber) {
        if (blockTimestamp < parentHeader.unixTimestamp + 13) 1 else -1
      } else {
        val timestampDiff = blockTimestamp - parentHeader.unixTimestamp
        math.max(1 - timestampDiff / 10, FrontierTimestampDiffLimit)
      }

    val extraDifficulty: BigInt =
      if (blockNumber < difficultyBombRemovalBlockNumber) {
        // calculate a fake block number for the ice-age delay https://github.com/ethereum/EIPs/blob/master/EIPS/eip-649.md
        val fakeBlockNumber: BigInt =
          if (blockNumber >= byzantiumBlockNumber) (blockNumber - RelaxDifficulty).max(0) else blockNumber

        val difficultyBombExponent = calculateBombExponent(fakeBlockNumber)
        if (difficultyBombExponent >= 0)
          BigInt(2).pow(difficultyBombExponent)
        else 0
      } else
        0

    val difficultyWithoutBomb = MinimumDifficulty.max(parentHeader.difficulty + x * c)
    difficultyWithoutBomb + extraDifficulty
  }

  private def calculateBombExponent(blockNumber: BigInt): Int = {
    if (blockNumber < difficultyBombPauseBlockNumber)
      (blockNumber / ExpDifficultyPeriod - 2).toInt
    else if (blockNumber < difficultyBombContinueBlockNumber)
      ((difficultyBombPauseBlockNumber / ExpDifficultyPeriod) - 2).toInt
    else {
      val delay = (difficultyBombContinueBlockNumber - difficultyBombPauseBlockNumber) / ExpDifficultyPeriod
      ((blockNumber / ExpDifficultyPeriod) - delay - 2).toInt
    }
  }
}
