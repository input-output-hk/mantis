package io.iohk.ethereum.consensus.pow.difficulty

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

class EthashDifficultyCalculator(blockchainConfig: BlockchainConfig) extends DifficultyCalculator {
  import blockchainConfig._
  import blockchainConfig.forkBlockNumbers._
  import DifficultyCalculator._

  private val ExpDifficultyPeriod: Int = 100000
  private val ByzantiumRelaxDifficulty: BigInt = 3000000
  private val ConstantinopleRelaxDifficulty: BigInt = 5000000

  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt = {
    lazy val timestampDiff = blockTimestamp - parentHeader.unixTimestamp

    val x: BigInt = parentHeader.difficulty / DifficultyBoundDivision
    val c: BigInt =
      if (blockNumber < homesteadBlockNumber) {
        if (blockTimestamp < parentHeader.unixTimestamp + 13) 1 else -1
      } else if (blockNumber >= byzantiumBlockNumber || blockNumber >= atlantisBlockNumber) {
        val parentUncleFactor = if (parentHeader.ommersHash == BlockHeader.EmptyOmmers) 1 else 2
        math.max(parentUncleFactor - (timestampDiff / 9), FrontierTimestampDiffLimit)
      } else {
        math.max(1 - (timestampDiff / 10), FrontierTimestampDiffLimit)
      }

    val extraDifficulty: BigInt =
      if (blockNumber < difficultyBombRemovalBlockNumber) {
        // calculate a fake block number for the ice-age delay
        val fakeBlockNumber: BigInt =
          // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1234.md
          if (blockNumber >= constantinopleBlockNumber) (blockNumber - ConstantinopleRelaxDifficulty).max(0)
          // https://github.com/ethereum/EIPs/blob/master/EIPS/eip-649.md
          else if (blockNumber >= byzantiumBlockNumber) (blockNumber - ByzantiumRelaxDifficulty).max(0)
          else blockNumber

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
