package io.iohk.ethereum.txExecTest

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.dsl.ResultOfATypeInvocation
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.domain.Receipt
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.Config

class ContractTest extends AnyFlatSpec with Matchers {
  val blockchainConfig = Config.blockchains.blockchainConfig
  val syncConfig: Config.SyncConfig = Config.SyncConfig(Config.config)
  val noErrors: ResultOfATypeInvocation[Right[_, Seq[Receipt]]] = a[Right[_, Seq[Receipt]]]

  "Ledger" should "execute and validate" in new ScenarioSetup {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")
    lazy val testBlockchainStorages = FixtureProvider.prepareStorages(2, fixtures)

    //block only with ether transfers
    override lazy val blockValidation =
      new BlockValidation(mining, blockchainReader, BlockQueue(blockchainReader, syncConfig))
    override lazy val blockExecution =
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        testBlockchainStorages.evmCodeStorage,
        mining.blockPreparator,
        blockValidation
      )
    blockExecution.executeAndBlock(fixtures.blockByNumber(1)) shouldBe noErrors

    // deploy contract
    blockExecution.executeAndBlock(fixtures.blockByNumber(2)) shouldBe noErrors

    // execute contract call
    // execute contract that pays 2 accounts
    blockExecution.executeAndBlock(fixtures.blockByNumber(3)) shouldBe noErrors
  }
}
