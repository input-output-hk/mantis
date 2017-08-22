package io.iohk.ethereum.ets.blockchain

import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.domain.Block.BlockDec
import io.iohk.ethereum.domain.{Account, Address, Block}
import io.iohk.ethereum.ets.common.AccountState
import io.iohk.ethereum.ledger.{BlockExecutionError, InMemoryWorldStateProxy, LedgerImpl}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.nodebuilder.{BlockchainConfigBuilder, ValidatorsBuilder}
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.{UInt256, VM}
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

abstract class ScenarioSetup(scenario: BlockchainScenario)
  extends EphemBlockchainTestSetup
  with ValidatorsBuilder
  with BlockchainConfigBuilder {

  val emptyWorld = blockchain.getWorldStateProxy(-1, UInt256.Zero, None)

  override lazy val blockchainConfig = buildBlockchainConfig(scenario.network)

  val ledger = new LedgerImpl(VM, blockchainConfig)

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

  def getInitialWorld(): InMemoryWorldStateProxy = {
    InMemoryWorldStateProxy.persistState(getWorldState(scenario.pre))
  }

  def getFinalWorld(): InMemoryWorldStateProxy = {
    InMemoryWorldStateProxy.persistState(getWorldState(scenario.postState))
  }

  def getBestBlock(): Option[Block] = {
    val bestBlockNumber = storagesInstance.storages.appStateStorage.getBestBlockNumber()
    blockchain.getBlockByNumber(bestBlockNumber)
  }

  private def buildBlockchainConfig(network: String): BlockchainConfig =  network match {
    case "EIP150"     => new Eip150Config
    case "Frontier"   => new FrontierConfig
    case "Homestead"  => new HomesteadConfig
    case "FrontierToHomesteadAt5" => new FrontierToHomesteadAt5
    case "HomesteadToEIP150At5" => new HomesteadToEIP150At5

    // Some default config, test will fail or be canceled
    case _ => new FrontierConfig
  }

  // Block rlp hex decoding, should be done during test and not parsing json as it can fail
  def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    Hex.decode(stripped)
  }

  def decodeBlock(s: String): Option[Block] = {
    Try(decode(s).toBlock) match {
      case Success(block) => Some(block)
      case Failure(ex) => {ex.printStackTrace(); None}
    }
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

  // Function copied from RegularSync.scala, as ledger itself do not persis schanges in blockchain
  // TODO We need to take forks and branches into account
  @tailrec
  final def processBlocks(blocks: Seq[Block], blockParentTd: BigInt,
                            newBlocks: Seq[NewBlock] = Nil, errors: Seq[BlockExecutionError] = Nil): (Seq[NewBlock], Seq[BlockExecutionError]) = blocks match {
    case Nil =>
      newBlocks -> errors

    case Seq(block, otherBlocks@_*) =>
      val blockHashToDelete = blockchain.getBlockHeaderByNumber(block.header.number).map(_.hash).filter(_ != block.header.hash)
      val blockExecResult = ledger.executeBlock(block, storagesInstance.storages, validators)
      blockExecResult match {
        case Right(receipts) =>
          blockchain.save(block)
          blockchain.save(block.header.hash, receipts)
          storagesInstance.storages.appStateStorage.putBestBlockNumber(block.header.number)
          val newTd = blockParentTd + block.header.difficulty
          blockchain.save(block.header.hash, newTd)
          blockHashToDelete.foreach(blockchain.removeBlock)
          processBlocks(otherBlocks, newTd, newBlocks :+ NewBlock(block, newTd))
        case Left(error) =>
          processBlocks(otherBlocks, blockParentTd, newBlocks, errors :+ error)
      }
  }

}
