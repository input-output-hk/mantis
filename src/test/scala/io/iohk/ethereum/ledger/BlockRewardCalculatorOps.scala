package io.iohk.ethereum.ledger

object BlockRewardCalculatorOps {

  implicit class BlockRewardCalculatorWithMinerReward(calculator: BlockRewardCalculator) {
    def calculateMiningReward(blockNumber: BigInt, numberOfOmmers: Int): BigInt = {
      val rewardForBlock = calculator.calculateMiningRewardForBlock(blockNumber)
      val rewardForOmmers = calculator.calculateMiningRewardForOmmers(blockNumber, numberOfOmmers)
      rewardForBlock + rewardForOmmers
    }
  }
}