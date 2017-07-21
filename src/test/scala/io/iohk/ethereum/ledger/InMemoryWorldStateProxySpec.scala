package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.{Account, Address, BlockchainImpl}
import io.iohk.ethereum.vm.{Generators, UInt256}
import org.scalatest.{FlatSpec, Matchers}
import org.spongycastle.util.encoders.Hex

class InMemoryWorldStateProxySpec extends FlatSpec with Matchers {

  "InMemoryWorldStateProxy" should "allow to create and retrieve an account" in new TestSetup {
    worldState.newEmptyAccount(address1).accountExists(address1) shouldBe true
  }

  "InMemoryWorldStateProxy" should "allow to save and retrieve code" in new TestSetup {
    val code = Generators.getByteStringGen(1, 100).sample.get
    worldState.saveCode(address1, code).getCode(address1) shouldEqual code
  }

  "InMemoryWorldStateProxy" should "allow to save and get storage" in new TestSetup {
    val addr = Generators.getUInt256Gen().sample.getOrElse(UInt256.MaxValue)
    val value = Generators.getUInt256Gen().sample.getOrElse(UInt256.MaxValue)

    val storage = worldState
      .getStorage(address1)
      .store(addr, value)

    worldState.saveStorage(address1, storage).getStorage(address1).load(addr) shouldEqual value
  }

  "InMemoryWorldStateProxy" should "allow to transfer value to other address" in new TestSetup {
    val account = Account(0, 100)
    val toTransfer = account.balance - 20
    val finalWorldState = worldState
      .saveAccount(address1, account)
      .newEmptyAccount(address2)
      .transfer(address1, address2, UInt256(toTransfer))

    finalWorldState.getGuaranteedAccount(address1).balance shouldEqual (account.balance - toTransfer)
    finalWorldState.getGuaranteedAccount(address2).balance shouldEqual toTransfer
  }

  "InMemoryWorldStateProxy" should "not store within contract store if value is zero" in new TestSetup {
    val account = Account(0, 100)
    val worldStateWithAnAccount = worldState.saveAccount(address1, account)
    val persistedWorldStateWithAnAccount = InMemoryWorldStateProxy.persistState(worldStateWithAnAccount)

    val persistedWithContractStorageValue = InMemoryWorldStateProxy.persistState(
      persistedWorldStateWithAnAccount.saveStorage(address1, worldState
        .getStorage(address1)
        .store(UInt256.One, UInt256.Zero)
      )
    )
    persistedWorldStateWithAnAccount.stateRootHash shouldEqual persistedWithContractStorageValue.stateRootHash
  }

  "InMemoryWorldStateProxy" should "storing a zero on a contract store position should remove it from the underlying tree" in new TestSetup {
    val account = Account(0, 100)
    val worldStateWithAnAccount = worldState.saveAccount(address1, account)
    val persistedWorldStateWithAnAccount = InMemoryWorldStateProxy.persistState(worldStateWithAnAccount)

    val persistedWithContractStorageValue = InMemoryWorldStateProxy.persistState(
      persistedWorldStateWithAnAccount.saveStorage(address1, worldState
        .getStorage(address1)
        .store(UInt256.One, UInt256.One)
      )
    )
    persistedWorldStateWithAnAccount.stateRootHash equals persistedWithContractStorageValue.stateRootHash shouldBe false

    val persistedWithZero = InMemoryWorldStateProxy.persistState(
      persistedWorldStateWithAnAccount.saveStorage(address1, worldState
        .getStorage(address1)
        .store(UInt256.One, UInt256.Zero)
      )
    )

    persistedWorldStateWithAnAccount.stateRootHash shouldEqual persistedWithZero.stateRootHash
  }

  "InMemoryWorldStateProxy" should "be able to persist changes and continue working after that" in new TestSetup {

    val account = Account(0, 100)
    val addr = UInt256.Zero
    val value = UInt256.MaxValue
    val code = ByteString(Hex.decode("deadbeefdeadbeefdeadbeef"))

    val validateInitialWorld = (ws: InMemoryWorldStateProxy) => {
      ws.accountExists(address1) shouldEqual true
      ws.accountExists(address2) shouldEqual true
      ws.getCode(address1) shouldEqual code
      ws.getStorage(address1).load(addr) shouldEqual value
      ws.getGuaranteedAccount(address1).balance shouldEqual 0
      ws.getGuaranteedAccount(address2).balance shouldEqual account.balance
    }

    // Update WS with some data
    val afterUpdatesWorldState = worldState
      .saveAccount(address1, account)
      .saveCode(address1, code)
      .saveStorage(address1, worldState
        .getStorage(address1)
        .store(addr, value))
      .newEmptyAccount(address2)
      .transfer(address1, address2, UInt256(account.balance))

    validateInitialWorld(afterUpdatesWorldState)

    // Persist and check
    val persistedWorldState = InMemoryWorldStateProxy.persistState(afterUpdatesWorldState)
    validateInitialWorld(persistedWorldState)

    // Create a new WS instance based on storages and new root state and check
    val newWorldState =  BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, Some(persistedWorldState.stateRootHash))
    validateInitialWorld(newWorldState)

    // Update this new WS check everything is ok
    val updatedNewWorldState = newWorldState.transfer(address2, address1, UInt256(account.balance))
    updatedNewWorldState.getGuaranteedAccount(address1).balance shouldEqual account.balance
    updatedNewWorldState.getGuaranteedAccount(address2).balance shouldEqual 0
    updatedNewWorldState.getStorage(address1).load(addr) shouldEqual value

    // Persist and check again
    val persistedNewWorldState = InMemoryWorldStateProxy.persistState(updatedNewWorldState)

    persistedNewWorldState.getGuaranteedAccount(address1).balance shouldEqual account.balance
    persistedNewWorldState.getGuaranteedAccount(address2).balance shouldEqual 0
    persistedNewWorldState.getStorage(address1).load(addr) shouldEqual value

  }

  it should "be able to do transfers with the same origin and destination" in new TestSetup {

    val account = Account(0, 100)
    val toTransfer = account.balance - 20
    val finalWorldState = worldState
      .saveAccount(address1, account)
      .transfer(address1, address1, UInt256(toTransfer))

    finalWorldState.getGuaranteedAccount(address1).balance shouldEqual account.balance

  }

  trait TestSetup extends EphemBlockchainTestSetup {

    val worldState = BlockchainImpl(storagesInstance.storages).getWorldStateProxy(-1, UInt256.Zero, None)

    val address1 = Address(0x123456)
    val address2 = Address(0xabcdef)
  }

}



