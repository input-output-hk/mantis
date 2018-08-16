package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.consensus.ethash.EthashConsensus
import io.iohk.ethereum.consensus.ethash.validators.EthashValidators
import io.iohk.ethereum.consensus.{ConsensusConfig, FullConsensusConfig, TestConsensus, ethash}
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{SharedEphemDataSources, Storages}
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.domain.Block.BlockDec
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ets.common.AccountState
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import org.bouncycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

object ScenarioSetup {
  val specificConfig = ethash.EthashConfig(Config.config)
  val fullConfig = FullConsensusConfig(ConsensusConfig(Config.config)(null), specificConfig)

  def loadEthashConsensus(vm: VMImpl, blockchain: BlockchainImpl, blockchainConfig: BlockchainConfig, validators: EthashValidators): ethash.EthashConsensus = {
    val consensus = EthashConsensus(vm, blockchain, blockchainConfig, fullConfig, validators)
    consensus
  }

  trait Pruning extends PruningModeComponent {
    override val pruningMode: PruningMode = ArchivePruning
  }


  def getBlockchain(): BlockchainImpl = {
    val storagesInstance = new SharedEphemDataSources with Pruning with Storages.DefaultStorages
    BlockchainImpl(storagesInstance.storages)
  }
}

abstract class ScenarioSetup(_vm: VMImpl, scenario: BlockchainScenario) {

  val (blockchainConfig, validators) = buildBlockchainConfig(scenario.network)

  //val validators = StdEthashValidators(blockchainConfig)
  val blockchain = ScenarioSetup.getBlockchain()

  val consensus: TestConsensus = ScenarioSetup.loadEthashConsensus(_vm, blockchain, blockchainConfig, validators)

  val emptyWorld = blockchain.getWorldStateProxy(-1, UInt256.Zero, None, false, true)

  val ledger = new LedgerImpl(blockchain, new BlockQueue(blockchain, 10, 10), blockchainConfig, consensus)

  def loadGenesis(): Block = {
    val genesisBlock = scenario.genesisRLP match {
      case Some(rlp) =>
        val block = rlp.toArray.toBlock
        assert(block.header == scenario.genesisBlockHeader.toBlockHeader,
          "decoded genesis block header did not match the expectation")
        block

      case None =>
        Block(scenario.genesisBlockHeader.toBlockHeader, BlockBody(Nil, Nil))
    }

    blockchain.save(genesisBlock)
    blockchain.save(genesisBlock.header.hash, Nil)
    blockchain.save(genesisBlock.header.hash, genesisBlock.header.difficulty)
    genesisBlock
  }

  val initialWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(getWorldState(scenario.pre))

  val finalWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy.persistState(getWorldState(scenario.postState))

  def getBestBlock(): Option[Block] = {
    val bestBlockNumber = blockchain.getBestBlockNumber()
    blockchain.getBlockByNumber(bestBlockNumber)
  }

  def getExpectedState(): List[(Address, Option[Account])] = {
    scenario.postState.map((addAcc) => addAcc._1 -> finalWorld.getAccount(addAcc._1)).toList
  }

  def getResultState(): List[(Address, Option[Account])] = {
    val bestBlockNumber = blockchain.getBestBlockNumber()
    scenario.postState.map(addAcc => addAcc._1 -> blockchain.getAccount(addAcc._1, bestBlockNumber)).toList
  }

  private def buildBlockchainConfig(network: String): (BlockchainConfig, EthashValidators) = network match {
    case "EIP150" => (Eip150Config, Validators.eip150Validators)
    case "Frontier" => (FrontierConfig, Validators.frontierValidators)
    case "Homestead" => (HomesteadConfig, Validators.homesteadValidators)
    case "FrontierToHomesteadAt5" => (FrontierToHomesteadAt5, Validators.frontierToHomesteadValidators)
    case "HomesteadToEIP150At5" => (HomesteadToEIP150At5, Validators.homesteadToEipValidators)
    case "EIP158" => (Eip158Config, Validators.eip158Validators)
    case "HomesteadToDaoAt5" => (HomesteadToDaoAt5, Validators.homeSteadtoDaoValidators)
    case "Byzantium" => (ByzantiumConfig, Validators.byzantiumValidators)
    case "Constantinople" => (ConstantinopleConfig, Validators.byzantiumValidators)
    case "EIP158ToByzantiumAt5" => (Eip158ToByzantiumAt5Config, Validators.eip158ToByzantiumValidators)
    // Some default config, test will fail or be canceled
    case _ => (FrontierConfig, Validators.frontierValidators)
  }

  private def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    Hex.decode(stripped)
  }

  // During decoding we cant expect some failures especially in bcInvalidRlPTests.json
  private def decodeBlock(s: String): Option[Block] = {
    Try(decode(s).toBlock) match {
      case Success(block) => Some(block)
      case Failure(ex) => {
        ex.printStackTrace(); None
      }
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
