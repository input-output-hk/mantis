package io.iohk.ethereum.txExecTest

import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.txExecTest.util.FixtureProvider
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.VM
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps

class ContractTest extends FlatSpec with Matchers {
  val blockchainConfig = BlockchainConfig(Config.config)

  val noErrors: Right[Nothing, Unit] = Right(())
  val validators = new Validators {
    val blockValidator: BlockValidator = BlockValidator
    val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
    val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
    val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
  }

  "Ledger" should "transfer ether" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(0, fixtures)

    //block only with ether transfers
    new LedgerImpl(VM, blockchainConfig).executeBlock(fixtures.blockByNumber(1), storage, validators) shouldBe noErrors
  }

  it should "deploy contract" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(1, fixtures)

    //contract creation
    new LedgerImpl(VM, blockchainConfig).executeBlock(fixtures.blockByNumber(2), storage, validators) shouldBe noErrors
  }

  it should "execute contract call" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(2, fixtures)

    //block with ether transfers and contract call
    new LedgerImpl(VM, blockchainConfig).executeBlock(fixtures.blockByNumber(3), storage, validators) shouldBe noErrors
  }

  it should "execute contract that pays 2 accounts" in {
    val fixtures: FixtureProvider.Fixture = FixtureProvider.loadFixtures("/txExecTest/purchaseContract")

    val storage = FixtureProvider.prepareStorages(2, fixtures)

    //block contains contract paying 2 accounts
    new LedgerImpl(VM, blockchainConfig).executeBlock(fixtures.blockByNumber(3), storage, validators) shouldBe noErrors
  }
}
