package io.iohk.ethereum.ledger

import io.iohk.ethereum.utils.MonetaryPolicyConfig

/** Calculates rewards for mining blocks and ommers.
  * https://github.com/ethereumproject/ECIPs/blob/master/ECIPs/ECIP-1039.md completely specifies eventual rounding issues.
  */
class BlockRewardCalculator(
    config: MonetaryPolicyConfig,
    byzantiumBlockNumber: BigInt,
    constantinopleBlockNumber: BigInt
) {

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

  /** Block reward for miner after Constantinople Fork */
  val newRewardAfterConstantinople: BigInt = config.firstEraConstantinopleReducedBlockReward

  /** Reward to the block miner for inclusion of ommers as a fraction of block reward (numerator) */
  val ommerInclusionRewardNumer: BigInt = 1

  /** Reward to the block miner for inclusion of ommers as a fraction of block reward (denominator) */
  val ommerInclusionRewardDenom: BigInt = 32

  /** Reward to the miner of an included ommer as a fraction of block reward (numerator).
    * For era 2+
    */
  val ommerMiningRewardNumer: BigInt = 1

  /** Reward to the miner of an included ommer as a fraction of block reward (denominator).
    * For era 2+
    */
  val ommerMiningRewardDenom: BigInt = 32

  /** Reward to the miner of an included ommer as a fraction of block reward (max numerator).
    * Different in the first era
    */
  val firstEraOmmerMiningRewardMaxNumer: BigInt = 7

  /** Reward to the miner of an included ommer as a fraction of block reward (denominator).
    * Different in the first era
    */
  val firstEraOmmerMiningRewardDenom: BigInt = 8

  /** Calculates the miner reward for the block, that is, without considering the ommers included
    *
    * @param blockNumber of the mined block
    * @return miner reward for the block
    */
  def calculateMiningRewardForBlock(blockNumber: BigInt): BigInt = {
    val era = eraNumber(blockNumber)
    val eraMultiplier = rewardReductionRateNumer.pow(era)
    val eraDivisor = rewardReductionRateDenom.pow(era)
    newBlockReward(blockNumber) * eraMultiplier / eraDivisor
  }

  /** Calculates the miner reward for the ommers included on the block
    *
    * @param blockNumber of the mined block
    * @param ommersCount the number of ommers on the block
    * @return miner reward for the block ommers
    */
  def calculateMiningRewardForOmmers(blockNumber: BigInt, ommersCount: Int): BigInt =
    calculateMiningRewardPerOmmer(blockNumber) * ommersCount

  /** Calculates the ommers reward for the ommers included on the block
    *
    * @param blockNumber of the mined block
    * @param ommerNumber the block number of the ommer
    * @return ommer reward
    */
  def calculateOmmerRewardForInclusion(blockNumber: BigInt, ommerNumber: BigInt): BigInt = {
    val era = eraNumber(blockNumber)

    if (era == 0) {
      val number = firstEraOmmerMiningRewardMaxNumer - (blockNumber - ommerNumber - 1)
      (newBlockReward(blockNumber) * number) / firstEraOmmerMiningRewardDenom
    } else
      calculateMiningRewardForBlock(blockNumber) * ommerMiningRewardNumer / ommerMiningRewardDenom
  }

  /** Calculates reward given to the miner for each ommer included in the block
    *
    * @param blockNumber mined block
    * @return reward given to the miner for each ommer included
    */
  private def calculateMiningRewardPerOmmer(blockNumber: BigInt): BigInt =
    calculateMiningRewardForBlock(blockNumber) * ommerInclusionRewardNumer / ommerInclusionRewardDenom

  /** era number counting from 0 */
  private def eraNumber(blockNumber: BigInt): Int =
    ((blockNumber - 1) / eraDuration).toInt

  /** Assign proper blockReward accounting Byzantium/Constantinople fork
    * https://github.com/ethereum/EIPs/blob/master/EIPS/eip-649.md
    * https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1234.md
    */
  private def newBlockReward(blockNumber: BigInt): BigInt =
    if (blockNumber >= constantinopleBlockNumber) newRewardAfterConstantinople
    else if (blockNumber >= byzantiumBlockNumber) newRewardAfterByzantium
    else firstEraBlockReward
}
