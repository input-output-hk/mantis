package io.iohk.ethereum.ledger

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.Account
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.vm.BlockchainConfigForEvm
import io.iohk.ethereum.vm.EvmConfig

class DeleteTouchedAccountsSpec extends AnyFlatSpec with Matchers {

  val blockchainConfig = Config.blockchains.blockchainConfig
  val syncConfig: SyncConfig = SyncConfig(Config.config)

  it should "delete no accounts when there are no touched accounts" in new TestSetup {
    val newWorld =
      InMemoryWorldStateProxy.persistState(consensus.blockPreparator.deleteEmptyTouchedAccounts(worldStatePostEIP161))
    accountAddresses.foreach(a => assert(newWorld.getAccount(a).isDefined))
    newWorld.stateRootHash shouldBe worldStatePostEIP161.stateRootHash
  }

  it should "delete no accounts when there are no empty touched accounts" in new TestSetup {
    val worldAfterTransfer = worldStatePostEIP161.transfer(validAccountAddress, validAccountAddress2, transferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val newWorld =
      InMemoryWorldStateProxy.persistState(consensus.blockPreparator.deleteEmptyTouchedAccounts(worldAfterTransfer))
    accountAddresses.foreach(a => assert(newWorld.getAccount(a).isDefined))
  }

  it should "delete touched empty account" in new TestSetup {
    val worldAfterTransfer =
      worldStatePostEIP161.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val newWorld =
      InMemoryWorldStateProxy.persistState(consensus.blockPreparator.deleteEmptyTouchedAccounts(worldAfterTransfer))

    (accountAddresses - validEmptyAccountAddress).foreach(a => assert(newWorld.getAccount(a).isDefined))
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.touchedAccounts.size shouldEqual 0
  }

  it should "delete touched empty account after transfer to self" in new TestSetup {
    val worldAfterTransfer =
      worldStatePostEIP161.transfer(validEmptyAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 1

    val newWorld =
      InMemoryWorldStateProxy.persistState(consensus.blockPreparator.deleteEmptyTouchedAccounts(worldAfterTransfer))

    (accountAddresses - validEmptyAccountAddress).foreach(a => assert(newWorld.getAccount(a).isDefined))
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.touchedAccounts.size shouldEqual 0
  }

  it should "not mark for deletion and delete any account pre EIP161" in new TestSetup {
    val worldAfterTransfer =
      worldStatePreEIP161.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 0

    val worldAfterPayingToMiner =
      consensus.blockPreparator.pay(validEmptyAccountAddress1, zeroTransferBalance, withTouch = true)(
        worldAfterTransfer
      )

    worldAfterPayingToMiner.touchedAccounts.size shouldEqual 0

    val newWorld =
      InMemoryWorldStateProxy.persistState(consensus.blockPreparator.deleteEmptyTouchedAccounts(worldAfterTransfer))

    accountAddresses.foreach(a => assert(newWorld.getAccount(a).isDefined))
  }

  it should "delete multiple touched empty accounts" in new TestSetup {
    val worldAfterTransfer =
      worldStatePostEIP161.transfer(validAccountAddress, validEmptyAccountAddress, zeroTransferBalance)
    worldAfterTransfer.touchedAccounts.size shouldEqual 2

    val worldAfterPayingToMiner =
      consensus.blockPreparator.pay(validEmptyAccountAddress1, zeroTransferBalance, withTouch = true)(
        worldAfterTransfer
      )

    worldAfterPayingToMiner.touchedAccounts.size shouldEqual 3

    val newWorld = InMemoryWorldStateProxy.persistState(
      consensus.blockPreparator.deleteEmptyTouchedAccounts(worldAfterPayingToMiner)
    )

    (accountAddresses -- Set(validEmptyAccountAddress, validEmptyAccountAddress1)).foreach { a =>
      assert(newWorld.getAccount(a).isDefined)
    }
    newWorld.getAccount(validEmptyAccountAddress) shouldBe None
    newWorld.getAccount(validEmptyAccountAddress1) shouldBe None
    newWorld.touchedAccounts.size shouldEqual 0
  }

  it should "not delete touched new account resulting from contract creation (initialised)" in new TestSetup {
    val worldAfterInitAndTransfer =
      worldStatePostEIP161
        .initialiseAccount(validCreatedAccountAddress)
        .transfer(validAccountAddress, validCreatedAccountAddress, zeroTransferBalance)

    worldAfterInitAndTransfer.touchedAccounts.size shouldEqual 2

    val newWorld = InMemoryWorldStateProxy.persistState(
      consensus.blockPreparator.deleteEmptyTouchedAccounts(worldAfterInitAndTransfer)
    )

    (accountAddresses + validCreatedAccountAddress).foreach(a => assert(newWorld.getAccount(a).isDefined))
    newWorld.touchedAccounts.size shouldEqual 0
  }

  // scalastyle:off magic.number
  trait TestSetup extends EphemBlockchainTestSetup {
    //+ cake overrides
    override lazy val vm: VMImpl = new MockVM()

    //- cake overrides

    val conf: BlockchainConfigForEvm = BlockchainConfigForEvm(blockchainConfig)
    val postEip161Config: EvmConfig = EvmConfig.PostEIP161ConfigBuilder(conf)
    val postEip160Config: EvmConfig = EvmConfig.PostEIP160ConfigBuilder(conf)

    val validAccountAddress: Address = Address(0xababab)
    val validAccountBalance = 10

    val validAccountAddress2: Address = Address(0xcdcdcd)
    val validAccountAddress3: Address = Address(0xefefef)
    val validEmptyAccountAddress: Address = Address(0xaaaaaa)
    val validEmptyAccountAddress1: Address = Address(0xbbbbbb)

    val validCreatedAccountAddress: Address = Address(0xcccccc)

    val accountAddresses: Set[Address] = Set(
      validAccountAddress,
      validAccountAddress2,
      validAccountAddress3,
      validEmptyAccountAddress,
      validEmptyAccountAddress1
    )

    val worldStateWithoutPersist: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      noEmptyAccounts = postEip161Config.noEmptyAccounts,
      ethCompatibleStorage = true
    )
      .saveAccount(validAccountAddress, Account(balance = validAccountBalance))
      .saveAccount(validAccountAddress2, Account(balance = 20))
      .saveAccount(validAccountAddress3, Account(balance = 30))
      .saveAccount(validEmptyAccountAddress, Account.empty())
      .saveAccount(validEmptyAccountAddress1, Account.empty())

    val worldStateWithoutPersistPreEIP161: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
      storagesInstance.storages.evmCodeStorage,
      blockchain.getBackingMptStorage(-1),
      (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
      UInt256.Zero,
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      noEmptyAccounts = postEip160Config.noEmptyAccounts,
      ethCompatibleStorage = true
    )
      .saveAccount(validAccountAddress, Account(balance = validAccountBalance))
      .saveAccount(validAccountAddress2, Account(balance = 20))
      .saveAccount(validAccountAddress3, Account(balance = 30))
      .saveAccount(validEmptyAccountAddress, Account.empty())
      .saveAccount(validEmptyAccountAddress1, Account.empty())

    val transferBalance = 5
    val zeroTransferBalance = 0

    val worldStatePostEIP161: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(worldStateWithoutPersist)
    val worldStatePreEIP161: InMemoryWorldStateProxy =
      InMemoryWorldStateProxy.persistState(worldStateWithoutPersistPreEIP161)

  }
}
