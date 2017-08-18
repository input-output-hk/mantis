package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.db.components.Storages.DefaultStorages
import io.iohk.ethereum.domain.Block._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ets.vm.{AccountState, TestOptions}
import io.iohk.ethereum.jsonrpc.JsonRpcError
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.ledger.{BlockExecutionError, InMemoryWorldStateProxy, Ledger, LedgerImpl}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.BigIntExtensionMethods._
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, MonetaryPolicyConfig}
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.{UInt256, VM}
import org.scalatest._
import org.spongycastle.util.encoders.Hex

import scala.annotation.tailrec
import scala.util.Try
// scalastyle:off
class BlockChainSuite extends FreeSpec with Matchers with Logger {

  case class Env(
                  emptyWorld: InMemoryWorldStateProxy,
                  blockchain: Blockchain,
                  validators: Validators,
                  ledger: Ledger,
                  dataSource: DefaultStorages
                )

  trait BlockChainTestConfig extends BlockchainConfig {

    val frontierBlockNumber: BigInt = Long.MaxValue
    val eip160BlockNumber: BigInt = Long.MaxValue
    val eip150BlockNumber: BigInt = Long.MaxValue
    val eip155BlockNumber: BigInt = Long.MaxValue
    val homesteadBlockNumber: BigInt = Long.MaxValue

    // unused
    override val difficultyBombPauseBlockNumber: BigInt = 3000000
    override val difficultyBombContinueBlockNumber: BigInt = 5000000
    override val chainId: Byte = 0x3d.toByte
    override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
    override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))
    override val daoForkBlockNumber: BigInt = Long.MaxValue
    override val daoForkBlockHash: ByteString = ByteString("unused")
    override val accountStartNonce: UInt256 = UInt256.Zero
  }

  class FrontierConfig extends BlockChainTestConfig {
    override val frontierBlockNumber = 0
  }
  class HomesteadConfig extends BlockChainTestConfig {
    override val homesteadBlockNumber = 0
  }
  class Eip150Config extends BlockChainTestConfig {
    override val eip150BlockNumber = 0
  }
  class Eip160Config extends BlockChainTestConfig {
    override val eip160BlockNumber = 0
  }

  private def getBlockChainConfig(networkName: String): BlockchainConfig = {
    networkName match {
      case "EIP150" => new Eip150Config
      case "EIP158" => new Eip160Config
      case "Frontier" => new FrontierConfig
      case "Homestead" => new HomesteadConfig
      // Case which covers all transition networks
      // It would we good to create customm config, and set blocks number in it
      // from block numbers in transtion scenarions
      case _ => new FrontierConfig
    }
  }

  override def run(testName: Option[String], args: Args): Status = {

    val options = TestOptions(args.configMap)
    val scenarios = BlockChainScenarioLoader.load(options)

    scenarios.take(3).foreach { group =>
      group.name - {
        for {
          (name, scenario) <- group.scenarios
          if options.isScenarioIncluded(name)
        } {
          val blockChainConfig = getBlockChainConfig(scenario.network)

          name in new BlockChainTestSuiteSetup(blockChainConfig) {
            log.debug(s"Running test: ${group.name}/$name")
            runScenario(scenario, env)
          }
        }
      }
    }

    runTests(testName, args)
  }

  private def runScenario(scenario: testBlockChainScenario, env: Env): Unit = {
    val genesisHeader = scenario.genesisBlockHeader
    val genesisRlp = scenario.genesisRLP
    loadGenesis(genesisHeader, genesisRlp, env)

    val postWorldState = geWorldState(scenario.postState, env.emptyWorld)
    val hash = postWorldState.stateRootHash

    val preWorldState = geWorldState(scenario.pre, env.emptyWorld)

    // Needed to make this public to use outside of ledger package
    val persistedState = InMemoryWorldStateProxy.persistState(preWorldState)

    val blocks = scenario.blocks.map(testBlock => decode(testBlock.rlp).toBlock)

    val proc: (Seq[NewBlock], Option[BlockExecutionError]) = processBlocks(blocks, genesisHeader.difficulty, env = env)

    val newBlocks = proc._1

    val accounts = scenario.postState.map(a => env.blockchain.getAccount(a._1, newBlocks.size)).toList
    val postWorldStateAccounts = postWorldState.accountsStateTrie.cache.values.toList


    //Checking Accounts and last block hash
    //Only covering happy cases, we need to also consider the test which are meant to fail
    accounts should contain theSameElementsAs postWorldStateAccounts
    val lastBlock = env.blockchain.getBlockByNumber(newBlocks.size)
    lastBlock shouldBe defined
    lastBlock.get.header.hash shouldEqual scenario.lastblockhash
  }

  private def extractBytes(input: String): Either[JsonRpcError, ByteString] =
    Try(ByteString(decode(input))).toEither.left.map(_ => InvalidParams())

  private def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    Hex.decode(normalized)
  }

  class BlockChainTestSuiteSetup(blockChainConfig: BlockchainConfig) extends EphemBlockchainTestSetup {
    val emptyWorld: InMemoryWorldStateProxy = blockchain.getWorldStateProxy(-1, UInt256.Zero, None)
    val validators = new Validators {
      val blockValidator: BlockValidator = BlockValidator
      val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockChainConfig)
      val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockChainConfig)
      val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockChainConfig)
    }
    val ledger = new LedgerImpl(VM, blockChainConfig)

    val env = Env(emptyWorld, blockchain, validators, ledger, storagesInstance)
  }

  private def geWorldState(accounts: Map[Address, AccountState], world: InMemoryWorldStateProxy): InMemoryWorldStateProxy = {
    def updateWorld(startWorld: InMemoryWorldStateProxy, a: (Address, AccountState)): InMemoryWorldStateProxy = {
      val address = a._1
      val account = Account(nonce = a._2.nonce.u256, balance = a._2.balance.u256)
      val worldWithAccountAndCode = startWorld.saveAccount(address, account).saveCode(address, a._2.code)
      val storage = worldWithAccountAndCode.getStorage(address)
      val updatedStorage = a._2.storage.foldLeft(storage)((world, storage) => world.store(storage._1, storage._2))
      worldWithAccountAndCode.saveStorage(address, updatedStorage)
    }

    accounts.foldLeft(world)(updateWorld)
  }

  def loadGenesis(genesisTestHeader: testBlockHeader, genesisRLP: Option[String], env: Env): Unit = {

    // Do not know why we have header and rlp
    val genesisBlock = genesisRLP.map(decode(_).toBlock)

    val genesisHeader = testHeaderToHeader(genesisTestHeader)


    env.blockchain.save(Block(genesisHeader, BlockBody(Nil, Nil)))
    env.blockchain.save(genesisHeader.hash, Nil)
    env.blockchain.save(genesisHeader.hash, genesisHeader.difficulty)
  }

  def testHeaderToHeader(genesisTestHeader: testBlockHeader): BlockHeader = {
    BlockHeader(
      genesisTestHeader.parentHash,
      genesisTestHeader.uncleHash,
      genesisTestHeader.coinbase,
      genesisTestHeader.stateRoot,
      genesisTestHeader.transactionsTrie,
      genesisTestHeader.receiptTrie,
      genesisTestHeader.bloom,
      genesisTestHeader.difficulty,
      genesisTestHeader.number,
      genesisTestHeader.gasLimit,
      genesisTestHeader.gasUsed,
      genesisTestHeader.timestamp,
      genesisTestHeader.extraData,
      genesisTestHeader.mixHash,
      genesisTestHeader.nonce

    )
  }

//  def checkIfException(testBlock: TestBlock): Boolean = {
//    if (
//        testBlock.expectExceptionByzantium.isDefined ||
//        testBlock.expectExceptionConstantinople.isDefined || testBlock.expectExceptionEIP150.isDefined ||
//    )
//  }

  // Function copied from RegularSync.scala, as ledger itself do not persis schanges in block chaine
  @tailrec
  private def processBlocks(blocks: Seq[Block], blockParentTd: BigInt,
                            newBlocks: Seq[NewBlock] = Nil, env: Env): (Seq[NewBlock], Option[BlockExecutionError]) = blocks match {
    case Nil =>
      newBlocks -> None

    case Seq(block, otherBlocks@_*) =>
      val blockHashToDelete = env.blockchain.getBlockHeaderByNumber(block.header.number).map(_.hash).filter(_ != block.header.hash)
      val blockExecResult = env.ledger.executeBlock(block, env.dataSource.storages, env.validators)
      blockExecResult match {
        case Right(receipts) =>
          env.blockchain.save(block)
          env.blockchain.save(block.header.hash, receipts)
          env.dataSource.storages.appStateStorage.putBestBlockNumber(block.header.number)
          val newTd = blockParentTd + block.header.difficulty
          env.blockchain.save(block.header.hash, newTd)
          blockHashToDelete.foreach(env.blockchain.removeBlock)
          processBlocks(otherBlocks, newTd, newBlocks :+ NewBlock(block, newTd),env)
        case Left(error) =>
          newBlocks -> Some(error)
      }
  }
}
