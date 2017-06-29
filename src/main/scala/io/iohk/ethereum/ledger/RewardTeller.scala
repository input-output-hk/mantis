package io.iohk.ethereum.ledger

import io.iohk.ethereum.utils.MonetaryPolicyConfig

/**
  * Calculates rewards for mining blocks and ommers.
  * Avoids floating point arithmetic. Because of that the formulas may look a bit unintuitive, but the important
  * thing here is that we want to defer any division to be a single and final operation
  */
class RewardTeller(config: MonetaryPolicyConfig) {
  /** Era duration in blocks */
  val eraDuration: BigInt = config.eraDuration

  /** Rate at which block and ommer rewards are reduced in successive eras (numerator) */
  val rewardReductionRateDenom: BigInt = BigDecimal(1 - config.rewardRedutionRate).precision * 10
  /** Rate at which block and ommer rewards are reduced in successive eras (denominator) */
  val rewardReductionRateNumer: BigInt = ((1 - config.rewardRedutionRate) * rewardReductionRateDenom.toDouble).toInt

  /** Base block reward in the first era */
  val firstEraBlockReward: BigInt = config.firstEraBlockReward

  /** Reward to the block miner for inclusion of ommers as a fraction of block reward (numerator) */
  val ommerInclusionRewardNumer: BigInt = 1
  /** Reward to the block miner for inclusion of ommers as a fraction of block reward (denominator) */
  val ommerInclusionRewardDenom: BigInt = 32

  /** Reward to the miner of an included ommer as a fraction of block reward (numerator).
    * For era 2+ */
  val ommerMiningRewardNumer: BigInt = 1
  /** Reward to the miner of an included ommer as a fraction of block reward (denominator).
    * For era 2+ */
  val ommerMiningRewardDenom: BigInt = 32

  /** Reward to the miner of an included ommer as a fraction of block reward (max numerator).
    * Different in the first era */
  val firstEraOmmerMiningRewardMaxNumer: BigInt = 7
  /** Reward to the miner of an included ommer as a fraction of block reward (denominator).
    * Different in the first era */
  val firstEraOmmerMiningRewardDenom: BigInt = 8


  def calcBlockMinerReward(blockNumber: BigInt, ommersCount: Int): BigInt = {
    val era = eraNumber(blockNumber)
    val eraMultiplier = rewardReductionRateNumer.pow(era)
    val eraDivisor = rewardReductionRateDenom.pow(era)

    val baseReward = firstEraBlockReward * eraMultiplier / eraDivisor
    val ommersReward = firstEraBlockReward * ommersCount * ommerInclusionRewardNumer * eraMultiplier /
      (ommerInclusionRewardDenom * eraDivisor)
    baseReward + ommersReward
  }

  def calcOmmerMinerReward(blockNumber: BigInt, ommerNumber: BigInt): BigInt = {
    val era = eraNumber(blockNumber)

    if (era == 0) {
      val numer = firstEraOmmerMiningRewardMaxNumer - (blockNumber - ommerNumber - 1)
      firstEraBlockReward * numer / firstEraOmmerMiningRewardDenom
    } else {
      val eraMultiplier = rewardReductionRateNumer.pow(era)
      val eraDivisor = rewardReductionRateDenom.pow(era)
      firstEraBlockReward * ommerMiningRewardNumer * eraMultiplier / (ommerMiningRewardDenom * eraDivisor)
    }
  }

  /** era number counting from 0 */
  private def eraNumber(blockNumber: BigInt): Int =
    ((blockNumber - 1) / eraDuration).toInt
}
