package io.iohk.ethereum.ets.blockchain

import java.util.concurrent.Executors

import io.iohk.ethereum.consensus.ethash.EthashConsensus
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.{ConsensusConfig, FullConsensusConfig, TestConsensus, ethash}
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{EphemDataSourceComponent, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.Block.BlockDec
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ets.common.AccountState
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object ScenarioSetup {
  val testContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
  val specificConfig = ethash.EthashConfig(Config.config)
  val fullConfig = FullConsensusConfig(ConsensusConfig(Config.config), specificConfig)

  def loadEthashConsensus(
      vm: VMImpl,
      blockchain: BlockchainImpl,
      blockchainConfig: BlockchainConfig,
      validators: ValidatorsExecutor
  ): ethash.EthashConsensus = {
    val consensus = EthashConsensus(vm, blockchain, blockchainConfig, fullConfig, validators)
    consensus
  }

  trait Pruning extends PruningModeComponent {
    override val pruningMode: PruningMode = ArchivePruning
  }

  def getBlockchain: BlockchainImpl = {
    val storagesInstance = new EphemDataSourceComponent with Pruning with Storages.DefaultStorages
    BlockchainImpl(storagesInstance.storages)
  }
}

abstract class ScenarioSetup(_vm: VMImpl, scenario: BlockchainScenario) {

  import BlockchainTestConfig._

  // according to: https://github.com/ethereum/tests/issues/480 only "NoProof" value should change our current implementation
  def shouldSkipPoW: Boolean = scenario.sealEngine.contains("NoProof")

  val (blockchainConfig, validators) = buildBlockchainConfig(scenario.network, shouldSkipPoW)

  //val validators = StdEthashValidators(blockchainConfig)
  val blockchain: BlockchainImpl = ScenarioSetup.getBlockchain

  val consensus: TestConsensus = ScenarioSetup.loadEthashConsensus(_vm, blockchain, blockchainConfig, validators)

  val emptyWorld: InMemoryWorldStateProxy =
    blockchain.getWorldStateProxy(-1, UInt256.Zero, None, noEmptyAccounts = false, ethCompatibleStorage = true)

  val ledger =
    new LedgerImpl(
      blockchain,
      new BlockQueue(blockchain, 50, 50),
      blockchainConfig,
      consensus,
      ScenarioSetup.testContext
    )

  def loadGenesis(): Block = {
    val genesisBlock = scenario.genesisRLP match {
      case Some(rlp) =>
        val block = rlp.toArray.toBlock
        assert(
          block.header == scenario.genesisBlockHeader.toBlockHeader,
          "decoded genesis block header did not match the expectation"
        )
        block

      case None =>
        Block(scenario.genesisBlockHeader.toBlockHeader, BlockBody(Nil, Nil))
    }

    blockchain
      .storeBlock(genesisBlock)
      .and(blockchain.storeReceipts(genesisBlock.header.hash, Nil))
      .and(blockchain.storeTotalDifficulty(genesisBlock.header.hash, genesisBlock.header.difficulty))
      .commit()

    genesisBlock
  }

  val initialWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(getWorldState(scenario.pre))

  val finalWorld: Option[InMemoryWorldStateProxy] =
    scenario.postState.map(postState => InMemoryWorldStateProxy.persistState(getWorldState(postState)))

  def getBestBlock: Option[Block] = {
    val bestBlockNumber = blockchain.getBestBlockNumber()
    blockchain.getBlockByNumber(bestBlockNumber)
  }

  def getExpectedState: Option[List[(Address, Option[Account])]] = {
    finalWorld.map(w => scenario.postState.get.map(addAcc => addAcc._1 -> w.getAccount(addAcc._1)).toList)
  }

  def getResultState: Option[List[(Address, Option[Account])]] = {
    val bestBlockNumber = blockchain.getBestBlockNumber()
    scenario.postState.map(_.map(addAcc => addAcc._1 -> blockchain.getAccount(addAcc._1, bestBlockNumber)).toList)
  }

  private def buildBlockchainConfig(network: String, shouldSkipPoW: Boolean): (BlockchainConfig, ValidatorsExecutor) = {
    if (shouldSkipPoW) withSkippedPoWValidationBlockchainConfig(network) else baseBlockchainConfig(network)
  }

  private def baseBlockchainConfig(network: String): (BlockchainConfig, ValidatorsExecutor) = network match {
    case "EIP150" => (Eip150Config, Validators.eip150Validators)
    case "Frontier" => (FrontierConfig, Validators.frontierValidators)
    case "Homestead" => (HomesteadConfig, Validators.homesteadValidators)
    case "FrontierToHomesteadAt5" => (FrontierToHomesteadAt5, Validators.frontierToHomesteadValidators)
    case "HomesteadToEIP150At5" => (HomesteadToEIP150At5, Validators.homesteadToEipValidators)
    case "EIP158" => (Eip158Config, Validators.eip158Validators)
    case "HomesteadToDaoAt5" => (HomesteadToDaoAt5, Validators.homesteadToDaoValidators)
    case "Byzantium" => (ByzantiumConfig, Validators.byzantiumValidators)
    case "Constantinople" => (ConstantinopleConfig, Validators.constantinopleValidators)
    case "EIP158ToByzantiumAt5" => (Eip158ToByzantiumAt5Config, Validators.eip158ToByzantiumValidators)
    case "ByzantiumToConstantinopleFixAt5" => (ByzantiumToConstantinopleAt5, Validators.byzantiumToConstantinopleAt5)
    case "ConstantinopleFix" => (ConstantinopleFixConfig, Validators.constantinopleValidators)
    case "Istanbul" => (IstanbulConfig, Validators.istanbulValidators)
    // Some default config, test will fail or be canceled
    case _ => (FrontierConfig, Validators.frontierValidators)
  }

  private def withSkippedPoWValidationBlockchainConfig(network: String): (BlockchainConfig, ValidatorsExecutor) =
    network match {
      case "EIP150" => (Eip150Config, ValidatorsWithSkippedPoW.eip150Validators)
      case "Frontier" => (FrontierConfig, ValidatorsWithSkippedPoW.frontierValidators)
      case "Homestead" => (HomesteadConfig, ValidatorsWithSkippedPoW.homesteadValidators)
      case "FrontierToHomesteadAt5" => (FrontierToHomesteadAt5, ValidatorsWithSkippedPoW.frontierToHomesteadValidators)
      case "HomesteadToEIP150At5" => (HomesteadToEIP150At5, ValidatorsWithSkippedPoW.homesteadToEipValidators)
      case "EIP158" => (Eip158Config, ValidatorsWithSkippedPoW.eip158Validators)
      case "HomesteadToDaoAt5" => (HomesteadToDaoAt5, ValidatorsWithSkippedPoW.homesteadToDaoValidators)
      case "Byzantium" => (ByzantiumConfig, ValidatorsWithSkippedPoW.byzantiumValidators)
      case "Constantinople" => (ConstantinopleConfig, ValidatorsWithSkippedPoW.constantinopleValidators)
      case "EIP158ToByzantiumAt5" => (Eip158ToByzantiumAt5Config, ValidatorsWithSkippedPoW.eip158ToByzantiumValidators)
      case "ByzantiumToConstantinopleFixAt5" =>
        (ByzantiumToConstantinopleAt5, ValidatorsWithSkippedPoW.byzantiumToConstantinopleAt5)
      case "ConstantinopleFix" => (ConstantinopleFixConfig, ValidatorsWithSkippedPoW.constantinopleFixValidators)
      case "Istanbul" => (IstanbulConfig, ValidatorsWithSkippedPoW.istanbulValidators)
      // Some default config, test will fail or be canceled
      case _ => (FrontierConfig, ValidatorsWithSkippedPoW.frontierValidators)
    }

  private def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    Hex.decode(stripped)
  }

  // During decoding we cant expect some failures especially in bcInvalidRlPTests.json
  private def decodeBlock(s: String): Option[Block] = {
    Try(decode(s).toBlock) match {
      case Success(block) =>
        Some(block)

      case Failure(ex) =>
        ex.printStackTrace()
        None
    }
  }

  private def isInvalidBlock(blockDef: BlockDef): Boolean = {
    blockDef.blockHeader.isEmpty && blockDef.transactions.isEmpty && blockDef.uncleHeaders.isEmpty
  }

  def getInvalid: List[BlockDef] = {
    scenario.blocks.filter(isInvalidBlock)
  }

  def getBlocks(blocks: List[BlockDef]): List[Block] = {
    blocks.flatMap(blockDef => decodeBlock(blockDef.rlp))
  }

  private def getWorldState(accounts: Map[Address, AccountState]): InMemoryWorldStateProxy = {
    accounts.foldLeft(emptyWorld) { case (world, (address, accountState)) =>
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
