package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Account, Address, BlockchainImpl, UInt256}
import io.iohk.ethereum.utils.{BlockchainConfig, Config, MonetaryPolicyConfig}
import io.iohk.ethereum.vm.EvmConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class DeleteTouchedAccountsSpec extends FlatSpec with Matchers with MockFactory {

  val blockchainConfig = BlockchainConfig(Config.config)

  val blockchain = mock[BlockchainImpl]

  val ledger = new LedgerImpl(new Mocks.MockVM(), blockchain, blockchainConfig)

  it should "delete no accounts when there are no touched accounts" in new TestSetup {
    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip161Config)(worldState))
    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.stateRootHash shouldBe worldState.stateRootHash
  }

  it should "delete no accounts when there are no empty touched accounts" in new TestSetup {
    val worldAfterTransfer = worldState.transfer(validAccountAddress, validAccountAddress2, transferBalance, postEip161Config.noEmptyAccounts)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip161Config)(worldAfterTransfer))
    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isDefined) }
  }

  it should "delete touched empty account" in new TestSetup {
    val worldAfterTransfer = worldState.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance, postEip161Config.noEmptyAccounts)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip161Config)(worldAfterTransfer))

    (accountAddresses - validEmptyAccountAddress).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
  }

  it should "delete touched empty account after transfer to self" in new TestSetup {
    val worldAfterTransfer = worldState.transfer(validEmptyAccountAddress, validEmptyAccountAddress, zeroTransferBalance, postEip161Config.noEmptyAccounts)
    worldAfterTransfer.touchedAccounts.size shouldEqual 1

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip161Config)(worldAfterTransfer))

    (accountAddresses - validEmptyAccountAddress).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
  }


  it should "not mark for deletion and delete any account pre EIP161" in new TestSetup {
    val worldAfterTransfer = worldState.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance, postEip160Config.noEmptyAccounts)
    worldAfterTransfer.touchedAccounts.size shouldEqual 0

    val worldAfterPayingToMiner = ledger.pay(validEmptyAccountAddress1, zeroTransferBalance, postEip160Config.noEmptyAccounts)(worldAfterTransfer)

    worldAfterPayingToMiner.touchedAccounts.size shouldEqual 0

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip160Config)(worldAfterTransfer))

    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isDefined) }
  }


  it should "delete multiple touched empty accounts" in new TestSetup {
    val worldAfterTransfer = worldState.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance, postEip161Config.noEmptyAccounts)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val worldAfterPayingToMiner = ledger.pay(validEmptyAccountAddress1, zeroTransferBalance, postEip161Config.noEmptyAccounts)(worldAfterTransfer)

    worldAfterPayingToMiner.touchedAccounts.size shouldEqual 3

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip161Config)(worldAfterPayingToMiner))

    (accountAddresses -- Set(validEmptyAccountAddress, validEmptyAccountAddress1)).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.getAccount(validEmptyAccountAddress1) shouldBe None
  }

  it should "delete multiple touched empty accounts more operations" in new TestSetup {
    val worldAfterTransfer = worldState.transfer(validAccountAddress3, validEmptyAccountAddress, zeroTransferBalance, postEip161Config.noEmptyAccounts)

    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val worldAfterPayingToMiner = ledger.pay(validEmptyAccountAddress1, zeroTransferBalance, postEip161Config.noEmptyAccounts)(worldAfterTransfer)

    worldAfterPayingToMiner.touchedAccounts.size shouldEqual 3

    val worldafterInitialisation =
      worldAfterPayingToMiner.initialiseAccount(validAccountAddress, validCreatedAccountAddress, validAccountBalance, postEip161Config.noEmptyAccounts)

    worldafterInitialisation.touchedAccounts.size shouldEqual 5

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip161Config)(worldafterInitialisation))

    (accountAddresses -- Set(validEmptyAccountAddress, validEmptyAccountAddress1, validAccountAddress) + validCreatedAccountAddress)
      .foreach{ a => assert(newWorld.getAccount(a).isDefined) }

    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.getAccount(validEmptyAccountAddress1) shouldBe None
    newWorld.getAccount(validAccountAddress) shouldBe None
  }


  it should "not delete touched account created by message transaction or createOp" in new TestSetup {
    val worldAfterTransfer =
      worldState.initialiseAccount(validAccountAddress, validCreatedAccountAddress, zeroTransferBalance, postEip161Config.noEmptyAccounts)

    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteTouchedAccounts(postEip161Config)(worldAfterTransfer))

    (accountAddresses + validCreatedAccountAddress).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
  }

  trait TestSetup extends EphemBlockchainTestSetup {

    val validAccountAddress  = Address(0xababab)
    val validAccountBalance = 10

    val validAccountAddress2 = Address(0xcdcdcd)
    val validAccountAddress3     = Address(0xefefef)
    val validEmptyAccountAddress = Address(0xaaaaaa)
    val validEmptyAccountAddress1 = Address(0xbbbbbb)

    val validCreatedAccountAddress = Address(0xcccccc)

    val accountAddresses = Set(validAccountAddress, validAccountAddress2, validAccountAddress3, validEmptyAccountAddress, validEmptyAccountAddress1)

    val worldStateWithoutPersist: InMemoryWorldStateProxy =
      BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None)
        .saveAccount(validAccountAddress, Account(balance = validAccountBalance))
        .saveAccount(validAccountAddress2, Account(balance = 20))
        .saveAccount(validAccountAddress3, Account(balance = 30))
        .saveAccount(validEmptyAccountAddress, Account.empty())
        .saveAccount(validEmptyAccountAddress1, Account.empty())


    val transferBalance = 5
    val zeroTransferBalance = 0

    val worldState = InMemoryWorldStateProxy.persistState(worldStateWithoutPersist)

    val postEip161Config = EvmConfig.PostEIP161Config
    val postEip160Config = EvmConfig.PostEIP160Config
  }
}
