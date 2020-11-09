package io.iohk.ethereum.consensus.ethash.difficulty

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.domain.BlockHeader

class TargetTimeDifficultyCalculator(powTargetTime: Long) extends DifficultyCalculator {

  import DifficultyCalculator._

  /**
    * The lowerBoundExpectedRatio (l for abbreviation below) divides the timestamp diff into ranges:
    *   [0, l)   => c = 1, difficulty increases
    *   [l, 2*l) => c = 0. difficulty stays the same
    *   ...
    *   [l*i, l*(i+1) ) => c = 1-i, difficulty decreases
    *
    *  example:
    *  powTargetTime := 45 seconds
    *  l := 30 seconds
    *   [0, 0.5 min)         => difficulty increases
    *   [0.5 min, 1 min)   => difficulty stays the same (the average should be powTargetTime)
    *   [1 min, +infinity) => difficulty decreases
    */
  private val lowerBoundExpectedRatio: Long = (powTargetTime / 1.5).toLong

  def calculateDifficulty(blockNumber: BigInt, blockTimestamp: Long, parentHeader: BlockHeader): BigInt = {
    val timestampDiff = blockTimestamp - parentHeader.unixTimestamp

    val x: BigInt = parentHeader.difficulty / DifficultyBoundDivision
    val c: BigInt = math.max(1 - (timestampDiff / lowerBoundExpectedRatio), FrontierTimestampDiffLimit)

    MinimumDifficulty.max(parentHeader.difficulty + x * c)
  }
}
