package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.{Account, Address, Block, BlockHeader}
import io.iohk.ethereum.ets.common.AccountState
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, LedgerImpl}
import io.iohk.ethereum.nodebuilder.{BlockchainConfigBuilder, ValidatorsBuilder}
import io.iohk.ethereum.rlp
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.{BlockchainConfig, MonetaryPolicyConfig}
import io.iohk.ethereum.vm.{UInt256, VM}

abstract class ScenarioSetup(scenario: Scenario)
  extends EphemBlockchainTestSetup
  with ValidatorsBuilder
  with BlockchainConfigBuilder {

  val emptyWorld = blockchain.getWorldStateProxy(-1, UInt256.Zero, None)

  override val blockchainConfig = buildBlockchainConfig(scenario.network)

  val ledger = new LedgerImpl(VM, blockchainConfig)

  def loadGenesis(): BlockHeader = {
    val genesisBlock = rlp.decode[Block](scenario.genesisRLP.toArray)
    blockchain.save(genesisBlock)
    genesisBlock.header
  }

  def prepareInitialWorld(): Unit =
    InMemoryWorldStateProxy.persistState(getWorldState(scenario.pre))

  lazy val expectedStateRoot = InMemoryWorldStateProxy.persistState(getWorldState(scenario.postState)).stateRootHash

  // TODO: build proper config based on network
  private def buildBlockchainConfig(network: String): BlockchainConfig =
    new BlockchainConfig {
      override val frontierBlockNumber: BigInt = 0
      override val homesteadBlockNumber: BigInt = 1150000
      override val difficultyBombPauseBlockNumber: BigInt = 3000000
      override val difficultyBombContinueBlockNumber: BigInt = 5000000
      override val eip155BlockNumber: BigInt = 0
      override val chainId: Byte = 0x3d.toByte
      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))

      // unused
      override val customGenesisFileOpt: Option[String] = None
      override val daoForkBlockNumber: BigInt = Long.MaxValue
      override val eip160BlockNumber: BigInt = Long.MaxValue
      override val eip150BlockNumber: BigInt = Long.MaxValue
      override val daoForkBlockHash: ByteString = ByteString("unused")
      override val accountStartNonce: UInt256 = UInt256.Zero
    }

  private def getWorldState(accounts: Map[Address, AccountState]): InMemoryWorldStateProxy = {
    scenario.pre.foldLeft(emptyWorld) { case (world, (address, accountState)) =>
      val account = Account(nonce = accountState.nonce.u256, balance = accountState.balance.u256)
      val worldWithAccountAndCode = world.saveAccount(address, account).saveCode(address, accountState.code)
      val emptyStorage = worldWithAccountAndCode.getStorage(address)
      val updatedStorage = accountState.storage.foldLeft(emptyStorage) { case (storage, (key, value)) =>
        storage.store(key, value)
      }
      worldWithAccountAndCode.saveStorage(address, updatedStorage)
    }
  }
}
