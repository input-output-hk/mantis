package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.domain.{BlockchainImpl, Receipt}
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{BlockchainConfig, Config, MonetaryPolicyConfig}
import org.scalatest.{FlatSpec, Matchers}

class ECIP1017Test extends FlatSpec with Matchers {

  val EraDuration = 3

  trait TestSetup extends ScenarioSetup {
    override lazy val blockchainConfig = BlockchainConfig(Config.config).copy(
      monetaryPolicyConfig = MonetaryPolicyConfig(EraDuration, 0.2, 5000000000000000000L))

    val noErrors = a[Right[_, Seq[Receipt]]]
  }

  val vm = new Ledger.VMImpl

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
