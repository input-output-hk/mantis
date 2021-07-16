package io.iohk.ethereum.consensus.difficulty

import io.iohk.ethereum.consensus.pow.difficulty.EthashDifficultyCalculator
import io.iohk.ethereum.consensus.pow.difficulty.TargetTimeDifficultyCalculator
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.utils.BlockchainConfig

trait DifficultyCalculator {
  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parent: BlockHeader)(implicit
      blockchainConfig: BlockchainConfig
  ): BigInt
}

object DifficultyCalculator extends DifficultyCalculator {

  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parent: BlockHeader)(implicit
      blockchainConfig: BlockchainConfig
  ): BigInt =
    (blockchainConfig.powTargetTime match {
      case Some(targetTime) => new TargetTimeDifficultyCalculator(targetTime)
      case None             => EthashDifficultyCalculator
    }).calculateDifficulty(blockNumber, blockTimestamp, parent)

  val DifficultyBoundDivision: Int = 2048
  val FrontierTimestampDiffLimit: Int = -99
  val MinimumDifficulty: BigInt = 131072
}
