package io.iohk.ethereum.ledger

import io.iohk.ethereum.utils.MonetaryPolicyConfig

/** Calculates rewards for mining blocks and ommers.
  * https://github.com/ethereumproject/ECIPs/blob/master/ECIPs/ECIP-1039.md completely specifies eventual rounding issues.
  */
class BlockRewardCalculator(config: MonetaryPolicyConfig, byzantiumBlockNumber: BigInt) {
  /** Era duration in blocks */
  val eraDuration: BigInt = config.eraDuration

  /** Rate at which block and ommer rewards are reduced in successive eras (numerator) */
  val rewardReductionRateDenom: BigInt = BigDecimal(1 - config.rewardReductionRate).precision * 10
  /** Rate at which block and ommer rewards are reduced in successive eras (denominator) */
  val rewardReductionRateNumer: BigInt = ((1 - config.rewardReductionRate) * rewardReductionRateDenom.toDouble).toInt

  /** Base block reward in the first era */
  val firstEraBlockReward: BigInt = config.firstEraBlockReward

  /** Block reward for miner after Byzantium Fork */
  val newRewardAfterByzantium: BigInt = config.firstEraReducedBlockReward

  /** Reward to the block miner for inclusion of ommers as a fraction of block reward (numerator) */
  val ommerInclusionRewardNumer: BigInt = 1
  /** Reward to the block miner for inclusion of ommers as a fraction of block reward (denominator) */
  val ommerInclusionRewardDenom: BigInt = 32

  /** Reward to the miner of an included ommer as a fraction of block reward (numerator). For era 2+ */
  val ommerMiningRewardNumer: BigInt = 1
  /** Reward to the miner of an included ommer as a fraction of block reward (denominator). For era 2+ */
  val ommerMiningRewardDenom: BigInt = 32

  /** Reward to the miner of an included ommer as a fraction of block reward (max numerator).
    * Different in the first era
    */
  val firstEraOmmerMiningRewardMaxNumer: BigInt = 7
  /** Reward to the miner of an included ommer as a fraction of block reward (denominator).
    * Different in the first era
    */
  val firstEraOmmerMiningRewardDenom: BigInt = 8


  def calcBlockMinerReward(blockNumber: BigInt, ommersCount: Int): BigInt = {
    val baseReward = calcMinerBaseReward(blockNumber)
    val ommersReward = calcMinerRewardPerOmmer(blockNumber) * ommersCount
    baseReward + ommersReward
  }

  def calcOmmerMinerReward(blockNumber: BigInt, ommerNumber: BigInt): BigInt = {
    val era = eraNumber(blockNumber)

    if (era == 0) {
      val number = firstEraOmmerMiningRewardMaxNumer - (blockNumber - ommerNumber - 1)
      (newBlockReward(blockNumber) * number) / firstEraOmmerMiningRewardDenom
    } else
      calcMinerBaseReward(blockNumber) * ommerMiningRewardNumer / ommerMiningRewardDenom
  }

  /** Calculates the miner base reward (without considering the ommers included)
    *
    * @param blockNumber mined block
    * @return miner base reward
    */
  private def calcMinerBaseReward(blockNumber: BigInt): BigInt = {
    val era = eraNumber(blockNumber)
    val eraMultiplier = rewardReductionRateNumer.pow(era)
    val eraDivisor = rewardReductionRateDenom.pow(era)
    newBlockReward(blockNumber) * eraMultiplier / eraDivisor
  }

  /** Calculates reward given to the miner for each ommer included in the block
    *
    * @param blockNumber mined block
    * @return reward given to the miner for each ommer included
    */
  private def calcMinerRewardPerOmmer(blockNumber: BigInt): BigInt = {
    calcMinerBaseReward(blockNumber) * ommerInclusionRewardNumer / ommerInclusionRewardDenom
  }

  /** Era number counting from 0 */
  private def eraNumber(blockNumber: BigInt): Int =
    ((blockNumber - 1) / eraDuration).toInt

  /** Assign proper blockReward accounting Byzantium fork
    * https://github.com/ethereum/EIPs/blob/master/EIPS/eip-649.md
    */
  private def newBlockReward(blockNumber: BigInt): BigInt =
    if (blockNumber >= byzantiumBlockNumber) newRewardAfterByzantium else firstEraBlockReward
}
