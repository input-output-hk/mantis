package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import org.scalatest.{FlatSpec, Matchers}

class ContractTest extends FlatSpec with Matchers {
  val blockchainConfig = BlockchainConfig(Config.config)
  val syncConfig = SyncConfig(Config.config)

  val noErrors = a[Right[_, Seq[Receipt]]]

  "Ledger" should "transfer ether" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(0, fixtures)

    //block only with ether transfers
    new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(1)) shouldBe noErrors
  }

  it should "deploy contract" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(1, fixtures)

    //contract creation
    new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(2)) shouldBe noErrors
  }

  it should "execute contract call" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(2, fixtures)

    //block with ether transfers and contract call
    new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }

  it should "execute contract that pays 2 accounts" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(2, fixtures)

    //block contains contract paying 2 accounts
    new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }
}
