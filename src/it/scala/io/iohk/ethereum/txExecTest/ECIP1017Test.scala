package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.domain.{BlockchainImpl, Receipt, UInt256}
import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{BlockchainConfig, DaoForkConfig, MonetaryPolicyConfig}
import org.scalatest.{FlatSpec, Matchers}

class ECIP1017Test extends FlatSpec with Matchers {

  val EraDuration = 3

  trait TestSetup extends ScenarioSetup {
    override lazy val blockchainConfig = new BlockchainConfig {
      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(EraDuration, 0.2, 5000000000000000000L)

      // unused
      override val maxCodeSize: Option[BigInt] = None
      override val chainId: Byte = 0x3d
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val eip106BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = 2500000
      override val eip160BlockNumber: BigInt = 3000000
      override val eip155BlockNumber: BigInt = 3000000
      override val eip161BlockNumber: BigInt = Long.MaxValue
      override val customGenesisFileOpt: Option[String] = None
      override val daoForkConfig: Option[DaoForkConfig] = None
      override val difficultyBombPauseBlockNumber: BigInt = Long.MaxValue
      override val difficultyBombContinueBlockNumber: BigInt = Long.MaxValue
      override val accountStartNonce: UInt256 = UInt256.Zero
      val gasTieBreaker: Boolean = false
    }

    val noErrors = a[Right[_, Seq[Receipt]]]
  }

  /**
    * Tests the block reward calculation through out all the monetary policy through all the eras till block
    * mining reward goes to zero. Block mining reward is tested till era 200 (that starts at block number 602)
    * as the reward reaches zero at era 193 (which starts at block number 579), given an eraDuration of 3,
    * a rewardReductionRate of 0.2 and a firstEraBlockReward of 5 ether.
    */
  "Ledger" should "execute blocks with respect to block reward changed by ECIP 1017" in new TestSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/ecip1017Test")

    val startBlock = 1
    val endBlock = 602

    protected val testBlockchainStorages = FixtureProvider.prepareStorages(startBlock, fixtures)

    (startBlock to endBlock) foreach { blockToExecute =>
      val storages = FixtureProvider.prepareStorages(blockToExecute - 1, fixtures)
      val blockchain = BlockchainImpl(storages)
      val ledger = new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus)

      ledger.executeBlock(fixtures.blockByNumber(blockToExecute)) shouldBe noErrors
    }
  }

}
