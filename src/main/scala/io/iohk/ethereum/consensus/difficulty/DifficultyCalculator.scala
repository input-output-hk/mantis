package io.iohk.ethereum.consensus.difficulty

import io.iohk.ethereum.consensus.ethash.difficulty.{EthashDifficultyCalculator, TargetTimeDifficultyCalculator}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

trait DifficultyCalculator {
  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parent: BlockHeader): BigInt
}

object DifficultyCalculator {
  def apply(blockchainConfig: BlockchainConfig): DifficultyCalculator =
    blockchainConfig.powTargetTime match {
      case Some(targetTime) => new TargetTimeDifficultyCalculator(targetTime)
      case None             => new EthashDifficultyCalculator(blockchainConfig)
    }

  val DifficultyBoundDivision: Int = 2048
  val FrontierTimestampDiffLimit: Int = -99
  val MinimumDifficulty: BigInt = 131072
}
