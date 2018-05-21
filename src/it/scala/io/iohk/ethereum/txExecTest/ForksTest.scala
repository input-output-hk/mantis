package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.domain.{BlockchainImpl, Receipt}
import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import org.scalatest.{FlatSpec, Matchers}

class ForksTest extends FlatSpec with Matchers {

  trait TestSetup extends ScenarioSetup {
    override lazy val blockchainConfig = BlockchainConfig(Config.config).copy(
       frontierBlockNumber = 0,
       homesteadBlockNumber = 3,
       eip150BlockNumber = 5,
       eip160BlockNumber = 7,
       eip155BlockNumber = 0,
       eip106BlockNumber = Long.MaxValue)

    val noErrors = a[Right[_, Seq[Receipt]]]
  }

  "Ledger" should "execute blocks with respect to forks" in new TestSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/forksTest")

    val startBlock = 1
    val endBlock = 11

    protected val testBlockchainStorages = FixtureProvider.prepareStorages(startBlock, fixtures)

    (startBlock to endBlock) foreach { blockToExecute =>
      val storages = FixtureProvider.prepareStorages(blockToExecute - 1, fixtures)
      val blockchain = BlockchainImpl(storages)
      val ledger = new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus)

      ledger.executeBlock(fixtures.blockByNumber(blockToExecute)) shouldBe noErrors
    }
  }

}
