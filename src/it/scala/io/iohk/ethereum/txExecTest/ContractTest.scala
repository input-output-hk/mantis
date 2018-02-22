package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.domain.{BlockchainImpl, Receipt}
import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.vm.VM
import org.scalatest.{FlatSpec, Matchers}

class ContractTest extends FlatSpec with Matchers {
  val blockchainConfig = BlockchainConfig(Config.config)
  val syncConfig = SyncConfig(Config.config)

  val noErrors = a[Right[_, Seq[Receipt]]]

  "Ledger" should "transfer ether" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(0, fixtures)
    val blockchain = BlockchainImpl(storage)

    //block only with ether transfers
    new LedgerImpl(VM, blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(1)) shouldBe noErrors
  }

  it should "deploy contract" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(1, fixtures)
    val blockchain = BlockchainImpl(storage)

    //contract creation
    new LedgerImpl(VM, blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(2)) shouldBe noErrors
  }

  it should "execute contract call" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(2, fixtures)
    val blockchain = BlockchainImpl(storage)

    //block with ether transfers and contract call
    new LedgerImpl(VM, blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }

  it should "execute contract that pays 2 accounts" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(2, fixtures)
    val blockchain = BlockchainImpl(storage)

    //block contains contract paying 2 accounts
    new LedgerImpl(VM, blockchain, blockchainConfig, syncConfig, consensus).executeBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }
}
