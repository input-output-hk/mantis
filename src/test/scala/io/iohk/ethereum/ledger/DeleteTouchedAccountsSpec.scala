package io.iohk.ethereum.ledger

import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Account, Address, BlockchainImpl, UInt256}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.vm.{EvmConfig, VM}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class DeleteTouchedAccountsSpec extends FlatSpec with Matchers with MockFactory {

  val blockchainConfig = BlockchainConfig(Config.config)
  val syncConfig = SyncConfig(Config.config)

  // FIXME Delete
  // val blockchain = mock[BlockchainImpl]

  it should "delete no accounts when there are no touched accounts" in new TestSetup {

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteEmptyTouchedAccounts(worldStatePostEIP161))
    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.stateRootHash shouldBe worldStatePostEIP161.stateRootHash
  }

  it should "delete no accounts when there are no empty touched accounts" in new TestSetup {

    val worldAfterTransfer = worldStatePostEIP161.transfer(validAccountAddress, validAccountAddress2, transferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteEmptyTouchedAccounts(worldAfterTransfer))
    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isDefined) }
  }

  it should "delete touched empty account" in new TestSetup {

    val worldAfterTransfer = worldStatePostEIP161.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteEmptyTouchedAccounts(worldAfterTransfer))

    (accountAddresses - validEmptyAccountAddress).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.touchedAccounts.size shouldEqual 0
  }

  it should "delete touched empty account after transfer to self" in new TestSetup {

    val worldAfterTransfer = worldStatePostEIP161.transfer(validEmptyAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 1

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteEmptyTouchedAccounts(worldAfterTransfer))

    (accountAddresses - validEmptyAccountAddress).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.touchedAccounts.size shouldEqual 0
  }


  it should "not mark for deletion and delete any account pre EIP161" in new TestSetup {

    val worldAfterTransfer = worldStatePreEIP161.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 0

    val worldAfterPayingToMiner = ledger.pay(validEmptyAccountAddress1, zeroTransferBalance)(worldAfterTransfer)

    worldAfterPayingToMiner.touchedAccounts.size shouldEqual 0

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteEmptyTouchedAccounts(worldAfterTransfer))

    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isDefined) }
  }


  it should "delete multiple touched empty accounts" in new TestSetup {

    val worldAfterTransfer = worldStatePostEIP161.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val worldAfterPayingToMiner = ledger.pay(validEmptyAccountAddress1, zeroTransferBalance)(worldAfterTransfer)

    worldAfterPayingToMiner.touchedAccounts.size shouldEqual 3

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteEmptyTouchedAccounts(worldAfterPayingToMiner))

    (accountAddresses -- Set(validEmptyAccountAddress, validEmptyAccountAddress1)).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.getAccount(validEmptyAccountAddress1) shouldBe None
    newWorld.touchedAccounts.size shouldEqual 0
  }

  it should "not delete touched new account resulting from contract creation (initialised)" in new TestSetup {

    val worldAfterInitAndTransfer =
      worldStatePostEIP161.initialiseAccount(validCreatedAccountAddress)
        .transfer(validAccountAddress, validCreatedAccountAddress, zeroTransferBalance)

    worldAfterInitAndTransfer.touchedAccounts.size shouldEqual 2

    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteEmptyTouchedAccounts(worldAfterInitAndTransfer))

    (accountAddresses + validCreatedAccountAddress).foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.touchedAccounts.size shouldEqual 0
  }

  // scalastyle:off magic.number
  trait TestSetup extends EphemBlockchainTestSetup {
    //+ cake overrides
    override lazy val vm: VM = new MockVM()

    // Give a more specific type to Ledger, it is needed by the tests
    override lazy val ledger: LedgerImpl = newLedger()
    //- cake overrides

    val postEip161Config = EvmConfig.PostEIP161ConfigBuilder(None)
    val postEip160Config = EvmConfig.PostEIP160ConfigBuilder(None)

    val validAccountAddress  = Address(0xababab)
    val validAccountBalance = 10

    val validAccountAddress2 = Address(0xcdcdcd)
    val validAccountAddress3     = Address(0xefefef)
    val validEmptyAccountAddress = Address(0xaaaaaa)
    val validEmptyAccountAddress1 = Address(0xbbbbbb)

    val validCreatedAccountAddress = Address(0xcccccc)

    val accountAddresses = Set(validAccountAddress, validAccountAddress2, validAccountAddress3, validEmptyAccountAddress, validEmptyAccountAddress1)

    val worldStateWithoutPersist: InMemoryWorldStateProxy =
      BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None, postEip161Config.noEmptyAccounts)
        .saveAccount(validAccountAddress, Account(balance = validAccountBalance))
        .saveAccount(validAccountAddress2, Account(balance = 20))
        .saveAccount(validAccountAddress3, Account(balance = 30))
        .saveAccount(validEmptyAccountAddress, Account.empty())
        .saveAccount(validEmptyAccountAddress1, Account.empty())

    val worldStateWithoutPersistPreEIP161: InMemoryWorldStateProxy =
      BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None, postEip160Config.noEmptyAccounts)
        .saveAccount(validAccountAddress, Account(balance = validAccountBalance))
        .saveAccount(validAccountAddress2, Account(balance = 20))
        .saveAccount(validAccountAddress3, Account(balance = 30))
        .saveAccount(validEmptyAccountAddress, Account.empty())
        .saveAccount(validEmptyAccountAddress1, Account.empty())


    val transferBalance = 5
    val zeroTransferBalance = 0

    val worldStatePostEIP161 = InMemoryWorldStateProxy.persistState(worldStateWithoutPersist)
    val worldStatePreEIP161 = InMemoryWorldStateProxy.persistState(worldStateWithoutPersistPreEIP161)

  }
}
