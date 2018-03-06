package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.Block.BlockDec
import io.iohk.ethereum.domain.{Account, Address, Block, UInt256}
import io.iohk.ethereum.ets.common.AccountState
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.nodebuilder.{ActorSystemBuilder, LedgerBuilder, SyncConfigBuilder}
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.VM
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

abstract class ScenarioSetup(_vm: VM, scenario: BlockchainScenario)
  extends EphemBlockchainTestSetup
  with SyncConfigBuilder
  with ActorSystemBuilder
  with LedgerBuilder {

  override lazy val vm = _vm

  override lazy val blockchainConfig = buildBlockchainConfig(scenario.network)

  val emptyWorld = blockchain.getWorldStateProxy(-1, UInt256.Zero, None)

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
    val bestBlockNumber = storagesInstance.storages.appStateStorage.getBestBlockNumber()
    blockchain.getBlockByNumber(bestBlockNumber)
  }

  def getExpectedState(): List[(Address, Option[Account])] = {
    scenario.postState.map((addAcc) => addAcc._1 -> finalWorld.getAccount(addAcc._1)).toList
  }

  def getResultState(): List[(Address, Option[Account])] = {
    val bestBlockNumber = storagesInstance.storages.appStateStorage.getBestBlockNumber()
    scenario.postState.map(addAcc => addAcc._1 -> blockchain.getAccount(addAcc._1, bestBlockNumber)).toList
  }

  private def buildBlockchainConfig(network: String): BlockchainConfig =  network match {
    case "EIP150"     => new Eip150Config
    case "Frontier"   => new FrontierConfig
    case "Homestead"  => new HomesteadConfig
    case "FrontierToHomesteadAt5" => new FrontierToHomesteadAt5
    case "HomesteadToEIP150At5" => new HomesteadToEIP150At5
    case "EIP158" => new Eip158Config
    case "HomesteadToDaoAt5" => new HomesteadToDaoAt5

    // Some default config, test will fail or be canceled
    case _ => new FrontierConfig
  }

  private def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    Hex.decode(stripped)
  }

  // During decoding we cant expect some failures especially in bcInvalidRlPTests.json
  private def decodeBlock(s: String): Option[Block] = {
    Try(decode(s).toBlock) match {
      case Success(block) => Some(block)
      case Failure(ex) => {ex.printStackTrace(); None}
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
