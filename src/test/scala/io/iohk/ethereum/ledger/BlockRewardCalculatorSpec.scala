package io.iohk.ethereum.ledger

import io.iohk.ethereum.utils.MonetaryPolicyConfig
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class BlockRewardCalculatorSpec extends FlatSpec with Matchers with PropertyChecks {

  "BlockRewardCalculator" should "correctly calculate block and ommer rewards" in {
    val standardMP = MonetaryPolicyConfig(5000000, 0.2, 5000000000000000000L)

    val testMP = MonetaryPolicyConfig(10, 0.5, 5000000)

    val table = Table[MonetaryPolicyConfig, BigInt, List[BigInt], BigInt, List[BigInt]](
      ("config", "blockNumber", "ommersNumbers", "expectedBlockReward", "expectedOmmersRewards"),
      (standardMP, 1, Nil, 5000000000000000000L, Nil),
      (standardMP, 1000000, List(999999), 5156250000000000000L, List(4375000000000000000L)),
      (standardMP, 5000000, List(4999998, 4999997), 5312500000000000000L, List(3750000000000000000L, 3125000000000000000L)),
      (standardMP, 5000000, Nil, 5000000000000000000L, Nil),
      (standardMP, 5000001, Nil, 4000000000000000000L, Nil),
      (standardMP, 7000000, List(6999999), 4125000000000000000L, List(125000000000000000L)),
      (standardMP, 10000000, List(9999998, 9999997), 4250000000000000000L, List(125000000000000000L, 125000000000000000L)),
      (standardMP, 20000000, List(19999998, 19999997), 2720000000000000000L, List(80000000000000000L, 80000000000000000L)),
      (standardMP, 20000001, List(19999998, 19999997), 2176000000000000000L, List(64000000000000000L, 64000000000000000L)),
      // era #193 is the last one where rewards for miners are non-zero
      (standardMP, 965000000, List(964999999, 964999999), 1, List(0, 0)),
      // era #194 - no rewards
      (standardMP, 965000001, List(964999999, 964999999), 0, List(0, 0)),
      (testMP, 10, List(9, 8), 5312500, List(4375000, 3750000)),
      (testMP, 11, List(9, 8), 2656250, List(78125, 78125)),
      (testMP, 20, Nil, 2500000, Nil),
      (testMP, 21, List(20), 1289062, List(39062))
    )

    forAll(table) { (config, blockNumber, ommersNumbers, expectedBlockReward, expectedOmmersRewards) =>
      val calculator = new BlockRewardCalculator(config)

      val blockReward = calculator.calcBlockMinerReward(blockNumber, ommersNumbers.size)
      val ommersRewards = ommersNumbers.map(calculator.calcOmmerMinerReward(blockNumber, _))

      blockReward shouldEqual expectedBlockReward
      ommersRewards shouldEqual expectedOmmersRewards
    }
  }
}
