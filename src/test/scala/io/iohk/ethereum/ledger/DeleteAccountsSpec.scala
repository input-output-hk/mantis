package io.iohk.ethereum.ledger

import io.iohk.ethereum.utils.{Config, BlockchainConfig}
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Account, Address}
import org.scalatest.{FlatSpec, Matchers}

class DeleteAccountsSpec extends FlatSpec with Matchers {

  val blockchainConfig = BlockchainConfig(Config.config)

  val ledger = new LedgerImpl(new Mocks.MockVM(), blockchainConfig)

  it should "delete no accounts when none of them should be deleted" in new TestSetup {
    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteAccounts(Nil)(worldState))
    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.stateRootHash shouldBe worldState.stateRootHash
  }

  it should "delete the accounts listed for deletion" in new TestSetup {
    val newWorld = ledger.deleteAccounts(accountAddresses.tail)(worldState)
    accountAddresses.tail.foreach{ a => assert(newWorld.getAccount(a).isEmpty) }
    assert(newWorld.getAccount(accountAddresses.head).isDefined)
  }

  it should "delete all the accounts if they are all listed for deletion" in new TestSetup {
    val newWorld = InMemoryWorldStateProxy.persistState(ledger.deleteAccounts(accountAddresses)(worldState))
    accountAddresses.foreach{ a => assert(newWorld.getAccount(a).isEmpty) }
    newWorld.stateRootHash shouldBe Account.EmptyStorageRootHash
  }

  trait TestSetup extends EphemBlockchainTestSetup {

    val validAccountAddress = Address(0xababab)
    val validAccountAddress2 = Address(0xcdcdcd)
    val validAccountAddress3 = Address(0xefefef)

    val accountAddresses = Seq(validAccountAddress, validAccountAddress2, validAccountAddress3)

    val worldStateWithoutPersist: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    ).saveAccount(validAccountAddress, Account(balance = 10))
      .saveAccount(validAccountAddress2, Account(balance = 20))
      .saveAccount(validAccountAddress3, Account(balance = 30))
    val worldState = InMemoryWorldStateProxy.persistState(worldStateWithoutPersist)
  }

}
