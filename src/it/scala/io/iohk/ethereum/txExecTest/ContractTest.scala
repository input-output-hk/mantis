package io.iohk.ethereum.txExecTest

import java.util.concurrent.Executors
import io.iohk.ethereum.domain.{BlockchainImpl, BlockchainStorages, Receipt}
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

  "Ledger" should "execute and validate" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")
    val testBlockchainStorages = FixtureProvider.prepareStorages(2, fixtures)

    //block only with ether transfers
    val blockValidation = new BlockValidation(consensus, blockchain, BlockQueue(blockchain, syncConfig))
    val blockExecution = new BlockExecution(
      blockchain,
      testBlockchainStorages.evmCodeStorage,
      blockchainConfig,
      consensus.blockPreparator,
      blockValidation
    )

    // transfer ether
    blockExecution.executeAndValidateBlock(fixtures.blockByNumber(1)) shouldBe noErrors

    // deploy contract
    blockExecution.executeAndValidateBlock(fixtures.blockByNumber(2)) shouldBe noErrors

    // execute contract call
    // execute contract that pays 2 accounts
    blockExecution.executeAndValidateBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }
}
