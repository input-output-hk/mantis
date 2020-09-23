package io.iohk.ethereum.ledger

object LedgerTestingUtils {
  def calculateFullMinerReward(blockRewardCalculator: BlockRewardCalculator, blockNumber: BigInt, numberOfOmmers: Int): BigInt = {
    val rewardForBlock = blockRewardCalculator.calculateMiningRewardForBlock(blockNumber)
    val rewardForOmmers = blockRewardCalculator.calculateMiningRewardForOmmers(blockNumber, numberOfOmmers)
    rewardForBlock + rewardForOmmers
  }
}
