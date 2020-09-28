package io.iohk.ethereum.ledger

object BlockRewardCalculatorOps {

  implicit class BlockRewardCalculatorWithFullReward(calculator: BlockRewardCalculator) {
    def calculateFullMinerReward(blockNumber: BigInt, numberOfOmmers: Int): BigInt = {
      val rewardForBlock = calculator.calculateMiningRewardForBlock(blockNumber)
      val rewardForOmmers = calculator.calculateMiningRewardForOmmers(blockNumber, numberOfOmmers)
      rewardForBlock + rewardForOmmers
    }
  }
}