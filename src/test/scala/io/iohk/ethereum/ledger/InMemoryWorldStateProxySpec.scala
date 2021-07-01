package io.iohk.ethereum.ledger

import akka.util.ByteString

import org.bouncycastle.util.encoders.Hex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MPTException
import io.iohk.ethereum.vm.EvmConfig
import io.iohk.ethereum.vm.Generators

class InMemoryWorldStateProxySpec extends AnyFlatSpec with Matchers {

  "InMemoryWorldStateProxy" should "allow to create and retrieve an account" in new TestSetup {
    worldState.newEmptyAccount(address1).accountExists(address1) shouldBe true
  }

  it should "allow to save and retrieve code" in new TestSetup {
    val code = Generators.getByteStringGen(1, 100).sample.get
    worldState.saveCode(address1, code).getCode(address1) shouldEqual code
  }

  it should "allow to save and get storage" in new TestSetup {
    val addr = Generators.getUInt256Gen().sample.getOrElse(UInt256.MaxValue).toBigInt
    val value = Generators.getUInt256Gen().sample.getOrElse(UInt256.MaxValue).toBigInt

    val storage = worldState
      .getStorage(address1)
      .store(addr, value)

    worldState.saveStorage(address1, storage).getStorage(address1).load(addr) shouldEqual value
  }

  it should "allow to transfer value to other address" in new TestSetup {
    val account = Account(0, 100)
    val toTransfer = account.balance - 20
    val finalWorldState = worldState
      .saveAccount(address1, account)
      .newEmptyAccount(address2)
      .transfer(address1, address2, UInt256(toTransfer))

    finalWorldState.getGuaranteedAccount(address1).balance shouldEqual (account.balance - toTransfer)
    finalWorldState.getGuaranteedAccount(address2).balance shouldEqual toTransfer
  }

  it should "not store within contract store if value is zero" in new TestSetup {
    val account = Account(0, 100)
    val worldStateWithAnAccount = worldState.saveAccount(address1, account)
    val persistedWorldStateWithAnAccount = InMemoryWorldStateProxy.persistState(worldStateWithAnAccount)

    val persistedWithContractStorageValue = InMemoryWorldStateProxy.persistState(
      persistedWorldStateWithAnAccount.saveStorage(
        address1,
        worldState
          .getStorage(address1)
          .store(UInt256.One, UInt256.Zero)
      )
    )
    persistedWorldStateWithAnAccount.stateRootHash shouldEqual persistedWithContractStorageValue.stateRootHash
  }

  it should "storing a zero on a contract store position should remove it from the underlying tree" in new TestSetup {
    val account = Account(0, 100)
    val worldStateWithAnAccount = worldState.saveAccount(address1, account)
    val persistedWorldStateWithAnAccount = InMemoryWorldStateProxy.persistState(worldStateWithAnAccount)

    val persistedWithContractStorageValue = InMemoryWorldStateProxy.persistState(
      persistedWorldStateWithAnAccount.saveStorage(
        address1,
        worldState
          .getStorage(address1)
          .store(UInt256.One, UInt256.One)
      )
    )
    persistedWorldStateWithAnAccount.stateRootHash.equals(
      persistedWithContractStorageValue.stateRootHash
    ) shouldBe false

    val persistedWithZero = InMemoryWorldStateProxy.persistState(
      persistedWorldStateWithAnAccount.saveStorage(
        address1,
        worldState
          .getStorage(address1)
          .store(UInt256.One, UInt256.Zero)
      )
    )

    persistedWorldStateWithAnAccount.stateRootHash shouldEqual persistedWithZero.stateRootHash
  }

  it should "be able to persist changes and continue working after that" in new TestSetup {

    val account = Account(0, 100)
    val addr = UInt256.Zero.toBigInt
    val value = UInt256.MaxValue.toBigInt
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
      .saveStorage(
        address1,
        worldState
          .getStorage(address1)
          .store(addr, value)
      )
      .newEmptyAccount(address2)
      .transfer(address1, address2, UInt256(account.balance))

    validateInitialWorld(afterUpdatesWorldState)

    // Persist and check
    val persistedWorldState = InMemoryWorldStateProxy.persistState(afterUpdatesWorldState)
    validateInitialWorld(persistedWorldState)

    // Create a new WS instance based on storages and new root state and check
    val newWorldState = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      persistedWorldState.stateRootHash,
      noEmptyAccounts = true,
      ethCompatibleStorage = true
    )

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

  it should "not allow transfer to create empty accounts post EIP161" in new TestSetup {
    val account = Account(0, 100)
    val zeroTransfer = UInt256.Zero
    val nonZeroTransfer = account.balance - 20

    val worldStateAfterEmptyTransfer = postEIP161WorldState
      .saveAccount(address1, account)
      .transfer(address1, address2, zeroTransfer)

    worldStateAfterEmptyTransfer.getGuaranteedAccount(address1).balance shouldEqual account.balance
    worldStateAfterEmptyTransfer.getAccount(address2) shouldBe None

    val finalWorldState = worldStateAfterEmptyTransfer.transfer(address1, address2, nonZeroTransfer)

    finalWorldState.getGuaranteedAccount(address1).balance shouldEqual account.balance - nonZeroTransfer

    val secondAccount = finalWorldState.getGuaranteedAccount(address2)
    secondAccount.balance shouldEqual nonZeroTransfer
    secondAccount.nonce shouldEqual UInt256.Zero
  }

  it should "correctly mark touched accounts post EIP161" in new TestSetup {
    val account = Account(0, 100)
    val zeroTransfer = UInt256.Zero
    val nonZeroTransfer = account.balance - 80

    val worldAfterSelfTransfer = postEIP161WorldState
      .saveAccount(address1, account)
      .transfer(address1, address1, nonZeroTransfer)

    val worldStateAfterFirstTransfer = worldAfterSelfTransfer
      .transfer(address1, address2, zeroTransfer)

    val worldStateAfterSecondTransfer = worldStateAfterFirstTransfer
      .transfer(address1, address3, nonZeroTransfer)

    worldStateAfterSecondTransfer.touchedAccounts should contain theSameElementsAs Set(address1, address3)
  }

  it should "update touched accounts using keepPrecompieContract method" in new TestSetup {
    val account = Account(0, 100)
    val zeroTransfer = UInt256.Zero
    val nonZeroTransfer = account.balance - 80

    val precompiledAddress = Address(3)

    val worldAfterSelfTransfer = postEIP161WorldState
      .saveAccount(precompiledAddress, account)
      .transfer(precompiledAddress, precompiledAddress, nonZeroTransfer)

    val worldStateAfterFirstTransfer = worldAfterSelfTransfer
      .saveAccount(address1, account)
      .transfer(address1, address2, zeroTransfer)

    val worldStateAfterSecondTransfer = worldStateAfterFirstTransfer
      .transfer(address1, address3, nonZeroTransfer)

    val postEip161UpdatedWorld = postEIP161WorldState.keepPrecompileTouched(worldStateAfterSecondTransfer)

    postEip161UpdatedWorld.touchedAccounts should contain theSameElementsAs Set(precompiledAddress)
  }

  it should "correctly determine if account is dead" in new TestSetup {
    val emptyAccountWorld = worldState.newEmptyAccount(address1)

    emptyAccountWorld.accountExists(address1) shouldBe true
    emptyAccountWorld.isAccountDead(address1) shouldBe true

    emptyAccountWorld.accountExists(address2) shouldBe false
    emptyAccountWorld.isAccountDead(address2) shouldBe true
  }

  it should "remove all ether from existing account" in new TestSetup {
    val startValue = 100

    val account = Account(UInt256.One, startValue)
    ByteString(Hex.decode("deadbeefdeadbeefdeadbeef"))

    val initialWorld = InMemoryWorldStateProxy.persistState(worldState.saveAccount(address1, account))

    val worldAfterEtherRemoval = initialWorld.removeAllEther(address1)

    val acc1 = worldAfterEtherRemoval.getGuaranteedAccount(address1)

    acc1.nonce shouldEqual UInt256.One
    acc1.balance shouldEqual UInt256.Zero
  }

  it should "get changed account from not persisted read only world" in new TestSetup {
    val account = Account(0, 100)

    val worldStateWithAnAccount = worldState.saveAccount(address1, account)

    val persistedWorldStateWithAnAccount = InMemoryWorldStateProxy.persistState(worldStateWithAnAccount)

    val readWorldState = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getReadOnlyMptStorage(),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      persistedWorldStateWithAnAccount.stateRootHash,
      noEmptyAccounts = false,
      ethCompatibleStorage = false
    )

    readWorldState.getAccount(address1) shouldEqual Some(account)

    val changedAccount = account.copy(balance = 90)

    val changedReadState = readWorldState
      .saveAccount(address1, changedAccount)

    val changedReadWorld = InMemoryWorldStateProxy.persistState(
      changedReadState
    )

    assertThrows[MPTException] {
      val newReadWorld = InMemoryWorldStateProxy(
        storagesInstance.storages.evmCodeStorage,
        blockchain.getReadOnlyMptStorage(),
        (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
        UInt256.Zero,
        changedReadWorld.stateRootHash,
        noEmptyAccounts = false,
        ethCompatibleStorage = false
      )

      newReadWorld.getAccount(address1) shouldEqual Some(changedAccount)
    }

    changedReadState.getAccount(address1) shouldEqual Some(changedAccount)
  }

  it should "properly handle address collision during initialisation" in new TestSetup {
    val alreadyExistingAddress = Address("0x6295ee1b4f6dd65047762f924ecd367c17eabf8f")
    val accountBalance = 100

    val callingAccount = Address("0xa94f5374fce5edbc8e2a8697c15331677e6ebf0b")

    val world1 = InMemoryWorldStateProxy.persistState(
      worldState
        .saveAccount(alreadyExistingAddress, Account.empty().increaseBalance(accountBalance))
        .saveAccount(callingAccount, Account.empty().increaseNonce())
        .saveStorage(alreadyExistingAddress, worldState.getStorage(alreadyExistingAddress).store(0, 1))
    )

    val world2 = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      world1.stateRootHash,
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )

    world2.getStorage(alreadyExistingAddress).load(0) shouldEqual 1

    val collidingAddress = world2.createAddress(callingAccount)

    collidingAddress shouldEqual alreadyExistingAddress

    val world3 = InMemoryWorldStateProxy.persistState(world2.initialiseAccount(collidingAddress))

    world3.getGuaranteedAccount(collidingAddress).balance shouldEqual accountBalance
    world3.getGuaranteedAccount(collidingAddress).nonce shouldEqual blockchainConfig.accountStartNonce
    world3.getStorage(collidingAddress).load(0) shouldEqual 0
  }

  trait TestSetup extends EphemBlockchainTestSetup {
    val postEip161Config: EvmConfig = EvmConfig.PostEIP161ConfigBuilder(io.iohk.ethereum.vm.Fixtures.blockchainConfig)

    val worldState: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )

    val postEIP161WorldState: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      noEmptyAccounts = postEip161Config.noEmptyAccounts,
      ethCompatibleStorage = false
    )

    val address1: Address = Address(0x123456)
    val address2: Address = Address(0xabcdef)
    val address3: Address = Address(0xfedcba)
  }
}
