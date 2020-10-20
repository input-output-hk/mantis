package io.iohk.ethereum.consensus.ethash.difficulty

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.consensus.ethash.difficulty.EthashDifficultyCalculator.{GrandparentsData, GrandparentsDataGetter}
import io.iohk.ethereum.domain.{BlockHeader, Checkpoint}
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefPostEcip1097
import io.iohk.ethereum.nodebuilder.BlockchainConfigBuilder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EthashDifficultyCalculatorSpec extends AnyFlatSpec with Matchers with BlockchainConfigBuilder {

  "EthashDifficultyCalculator" should "properly calculate the difficulty for block whose parent has checkpoint" in new TestSetup {
    val diff = 6
    val parentHeader: BlockHeader = validParentHeader.copy(
      extraFields = HefPostEcip1097(treasuryOptOut = false, Some(Checkpoint(Nil)))
    )
    val parentHeaderWithoutCheckpoint: BlockHeader = validParentHeader
    val grandparentHeader =
      Fixtures.Blocks.ValidBlock.header.copy(
        unixTimestamp = parentHeader.unixTimestamp - diff,
        number = parentHeader.number - 1,
        difficulty = parentHeader.difficulty - 100
      )
    val greatGrandparentHeader =
      Fixtures.Blocks.ValidBlock.header.copy(
        unixTimestamp = grandparentHeader.unixTimestamp - diff,
        number = grandparentHeader.number - 1,
        difficulty = grandparentHeader.difficulty - 100
      )

    override def grandparentsDataGetter: GrandparentsDataGetter = _ =>
      GrandparentsData(grandparentHeader.difficulty, grandparentHeader.unixTimestamp - greatGrandparentHeader.unixTimestamp)

    val blockNumber: BigInt = parentHeader.number + 1
    val blockTimestamp: Long = parentHeader.unixTimestamp + 6

    val difficultyWithCheckpoint: BigInt = difficultyCalculator.calculateDifficulty(blockNumber, blockTimestamp, parentHeader)
    val difficultyWithoutCheckpoint: BigInt = difficultyCalculator.calculateDifficulty(blockNumber, blockTimestamp, parentHeaderWithoutCheckpoint)

    difficultyWithCheckpoint should be < difficultyWithoutCheckpoint
  }

  it should "properly calculate the difficulty after difficulty bomb resume (with reward reduction)" in new TestSetup {
    val parentHeader: BlockHeader =
      validParentHeader.copy(number = 5000101, unixTimestamp = 1513175023, difficulty = BigInt("22627021745803"))

    val blockNumber: BigInt = parentHeader.number + 1
    val blockTimestamp: Long = parentHeader.unixTimestamp + 6

    val difficulty: BigInt = difficultyCalculator.calculateDifficulty(blockNumber, blockTimestamp, parentHeader)
    val expected = BigInt("22638070358408")

    difficulty shouldBe expected
  }

  it should "properly calculate the difficulty after difficulty defuse" in new TestSetup {
    val parentHeader: BlockHeader =
      validParentHeader.copy(number = 5899999, unixTimestamp = 1525176000, difficulty = BigInt("22627021745803"))

    val blockNumber: BigInt = parentHeader.number + 1
    val blockTimestamp: Long = parentHeader.unixTimestamp + 6

    val difficulty: BigInt = difficultyCalculator.calculateDifficulty(blockNumber, blockTimestamp, parentHeader)
    val blockDifficultyWihtoutBomb = BigInt("22638070096264")

    difficulty shouldBe blockDifficultyWihtoutBomb
  }

  it should "properly calculate a block after block reward reduction (without uncles)" in new TestSetup {
    val parentHeader: BlockHeader =
      validParentHeader.copy(number = 5863374, unixTimestamp = 1530104893, difficulty = BigInt("3480699544328087"))

    val blockNumber: BigInt = parentHeader.number + 1
    val blockTimestamp: Long = parentHeader.unixTimestamp + 6

    val difficulty: BigInt = difficultyCalculator.calculateDifficulty(blockNumber, blockTimestamp, parentHeader)

    /** Expected calculations:
      * blockNumber = 5863375 // < 5900000
      * timestampDiff = 6
      * x = 3480699544328087 / 2048 =
      * c = (1 - (6 / 9)) = 0,33  // > -99
      * fakeBlockNumber = 5863375 - 3000000 = 2863375
      * extraDifficulty = 134217728
      * difficultyWithoutBomb = 3480699544328087 + 1699560324378,95 * 0,33 = 3481260399235132
      */
    val blockDifficultyAfterRewardReduction = BigInt("3482399171761329")

    difficulty shouldBe blockDifficultyAfterRewardReduction
  }

  trait TestSetup {
    def grandparentsDataGetter: GrandparentsDataGetter = _ => GrandparentsData(0, 0)
    val difficultyCalculator = new EthashDifficultyCalculator(blockchainConfig, grandparentsDataGetter)
    val validParentHeader = Fixtures.Blocks.ValidBlock.header
  }

}
