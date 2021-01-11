package io.iohk.ethereum.txExecTest

import java.util.concurrent.Executors

import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.{BlockExecution, BlockQueue, BlockValidation, Ledger}
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.Config
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

class ContractTest extends AnyFlatSpec with Matchers {
  val blockchainConfig = Config.blockchains.blockchainConfig
  val syncConfig = Config.SyncConfig(Config.config)
  val vm = new Ledger.VMImpl
  val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
  val noErrors = a[Right[_, Seq[Receipt]]]

  "Ledger" should "transfer ether" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(0, fixtures)

    //block only with ether transfers
    val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
    val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)
    blockExecution.executeAndValidateBlock(fixtures.blockByNumber(1)) shouldBe noErrors
  }

  it should "deploy contract" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(1, fixtures)

    //contract creation
    val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
    val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)
    blockExecution.executeAndValidateBlock(fixtures.blockByNumber(2)) shouldBe noErrors
  }

  it should "execute contract call" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(2, fixtures)

    //block with ether transfers and contract call
    val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
    val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)
    blockExecution.executeAndValidateBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }

  it should "execute contract that pays 2 accounts" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val testBlockchainStorages = FixtureProvider.prepareStorages(2, fixtures)

    //block contains contract paying 2 accounts
    val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
    val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation)
    blockExecution.executeAndValidateBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }
}
