package io.iohk.ethereum.ledger

import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm.{DataWord, Generators}
import org.scalatest.{FlatSpec, Matchers}

class InMemoryWorldStateProxySpec extends FlatSpec with Matchers {

  "InMemoryWorldStateProxy" should "allow to create and retrieve an account" in new TestSetup {
    worldState.newEmptyAccount(address1).accountExists(address1) shouldBe true
  }

  "InMemoryWorldStateProxy" should "allow to save and retrieve code" in new TestSetup {
    val code = Generators.getByteStringGen(1, 100).sample.get
    worldState.saveCode(address1, code).getCode(address1) shouldEqual code
  }

  "InMemoryWorldStateProxy" should "allow to save and get storage" in new TestSetup {
    val addr = Generators.dataWordGen.sample.getOrElse(DataWord.MaxValue)
    val value = Generators.dataWordGen.sample.getOrElse(DataWord.MaxValue)

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
      .transfer(address1, address2, toTransfer)

    finalWorldState.getGuaranteedAccount(address1).balance shouldEqual (account.balance - toTransfer)
    finalWorldState.getGuaranteedAccount(address2).balance shouldEqual toTransfer
  }

  "InMemoryWorldStateProxy" should "be able to persist changes and continue working after that" in new TestSetup {

    val account = Account(0, 100)
    val addr = Generators.dataWordGen.sample.getOrElse(DataWord.MaxValue)
    val value = Generators.dataWordGen.sample.getOrElse(DataWord.MaxValue)
    val code = Generators.getByteStringGen(1, 100).sample.get

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
      .transfer(address1, address2, account.balance)

    validateInitialWorld(afterUpdatesWorldState)

    // Persist and check
    val persistedWorldState = InMemoryWorldStateProxy.persist(afterUpdatesWorldState)
    validateInitialWorld(persistedWorldState)

    // Create a new WS instance based on storages and new root state and check
    val newWorldState = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage,
      Some(persistedWorldState.stateRootHash)
    )
    validateInitialWorld(newWorldState)

    // Update this new WS check everything is ok
    val updatedNewWorldState = newWorldState.transfer(address2, address1, account.balance)
    updatedNewWorldState.getGuaranteedAccount(address1).balance shouldEqual account.balance
    updatedNewWorldState.getGuaranteedAccount(address2).balance shouldEqual 0
    persistedNewWorldState.getStorage(address1).load(addr) shouldEqual value

    // Persist and check again
    val persistedNewWorldState = InMemoryWorldStateProxy.persist(updatedNewWorldState)
    persistedNewWorldState.getGuaranteedAccount(address1).balance shouldEqual account.balance
    persistedNewWorldState.getGuaranteedAccount(address2).balance shouldEqual 0
    persistedNewWorldState.getStorage(address1).load(addr) shouldEqual value

  }

  trait TestSetup {
    val storagesInstance = new SharedEphemDataSources with Storages.DefaultStorages

    val worldState = InMemoryWorldStateProxy(
      storagesInstance.storages,
      storagesInstance.storages.nodeStorage
    )

    val address1 = Address(0x123456)
    val address2 = Address(0xabcdef)
  }

}



