package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Account, Address, BlockchainImpl, UInt256}
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DeleteAccountsSpec extends AnyFlatSpec with Matchers with MockFactory {

  val blockchainConfig = Config.blockchains.blockchainConfig
  val syncConfig = SyncConfig(Config.config)

  val blockchain = mock[BlockchainImpl]

  it should "delete no accounts when none of them should be deleted" in new TestSetup {
    val newWorld = InMemoryWorldStateProxy.persistState(consensus.blockPreparator.deleteAccounts(Set.empty)(worldState))
    accountAddresses.foreach { a => assert(newWorld.getAccount(a).isDefined) }
    newWorld.stateRootHash shouldBe worldState.stateRootHash
  }

  it should "delete the accounts listed for deletion" in new TestSetup {
    val newWorld = consensus.blockPreparator.deleteAccounts(accountAddresses.tail)(worldState)
    accountAddresses.tail.foreach { a => assert(newWorld.getAccount(a).isEmpty) }
    assert(newWorld.getAccount(accountAddresses.head).isDefined)
  }

  it should "delete all the accounts if they are all listed for deletion" in new TestSetup {
    val newWorld =
      InMemoryWorldStateProxy.persistState(consensus.blockPreparator.deleteAccounts(accountAddresses)(worldState))
    accountAddresses.foreach { a => assert(newWorld.getAccount(a).isEmpty) }
    newWorld.stateRootHash shouldBe Account.EmptyStorageRootHash
  }

  // scalastyle:off magic.number
  it should "delete account that had storage updated before" in new TestSetup {
    val worldStateWithStorage = worldState.saveStorage(
      validAccountAddress,
      worldState.getStorage(validAccountAddress).store(UInt256(1), UInt256(123))
    )

    val updatedWorldState = consensus.blockPreparator.deleteAccounts(accountAddresses)(worldStateWithStorage)

    val newWorld = InMemoryWorldStateProxy.persistState(updatedWorldState)
    assert(newWorld.getAccount(validAccountAddress).isEmpty)
  }

  // scalastyle:off magic.number
  trait TestSetup extends EphemBlockchainTestSetup {
    //+ cake overrides
    override lazy val vm: VMImpl = new MockVM()

    override lazy val ledger: LedgerImpl = newLedger()
    //- cake overrides

    val validAccountAddress = Address(0xababab)
    val validAccountAddress2 = Address(0xcdcdcd)
    val validAccountAddress3 = Address(0xefefef)

    val accountAddresses = Set(validAccountAddress, validAccountAddress2, validAccountAddress3)

    val worldStateWithoutPersist: InMemoryWorldStateProxy =
      BlockchainImpl(storagesInstance.storages)
        .getWorldStateProxy(
          -1,
          UInt256.Zero,
          ByteString(MerklePatriciaTrie.EmptyRootHash),
          noEmptyAccounts = false,
          ethCompatibleStorage = true
        )
        .saveAccount(validAccountAddress, Account(balance = 10))
        .saveAccount(validAccountAddress2, Account(balance = 20))
        .saveAccount(validAccountAddress3, Account(balance = 30))
    val worldState = InMemoryWorldStateProxy.persistState(worldStateWithoutPersist)
  }

}
