package io.iohk.ethereum.ets.blockchain

import akka.util.ByteString
import io.iohk.ethereum.db.components.{SharedEphemDataSources, SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ets.vm.{AccountState, Scenario, TestOptions}
import io.iohk.ethereum.utils.{BlockchainConfig, Config, Logger, MonetaryPolicyConfig}
import org.scalatest._
import io.iohk.ethereum.domain.Block._
import io.iohk.ethereum.jsonrpc.JsonRpcError
import io.iohk.ethereum.jsonrpc.JsonRpcErrors.InvalidParams
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, Ledger, LedgerImpl}
import io.iohk.ethereum.nodebuilder.PruningConfigBuilder
import io.iohk.ethereum.validators._
import io.iohk.ethereum.vm.{UInt256, VM}
import org.spongycastle.util.encoders.Hex
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.BigIntExtensionMethods._

import scala.util.{Failure, Success, Try}

// scalastyle:off
class BlockChainSuite extends FreeSpec with Matchers with Logger {

  case class Env(
                  emptyWorld: InMemoryWorldStateProxy,
                  blockchain: Blockchain,
                  validators: Validators,
                  ledger: Ledger,
                  dataSource: BlockchainStorages
                )

  override def run(testName: Option[String], args: Args): Status = {

    val options = TestOptions(args.configMap)
    val scenarios = BlockChainScenarioLoader.load(options)


    //    lazy val blockchainConfig = new BlockchainConfig {
    //      override val frontierBlockNumber: BigInt = 0
    //      override val homesteadBlockNumber: BigInt = 1150000
    //      override val difficultyBombPauseBlockNumber: BigInt = 3000000
    //      override val difficultyBombContinueBlockNumber: BigInt = 5000000
    //      override val eip155BlockNumber: BigInt = 0
    //      override val chainId: Byte = 0x3d.toByte
    //      override val customGenesisFileOpt: Option[String] = Some("test-genesis.json")
    //      override val monetaryPolicyConfig: MonetaryPolicyConfig = MonetaryPolicyConfig(5000000, 0.2, BigInt("5000000000000000000"))
    //
    //      // unused
    //      override val daoForkBlockNumber: BigInt = Long.MaxValue
    //      override val eip160BlockNumber: BigInt = Long.MaxValue
    //      override val eip150BlockNumber: BigInt = Long.MaxValue
    //      override val daoForkBlockHash: ByteString = ByteString("unused")
    //      override val accountStartNonce: UInt256 = UInt256.Zero
    //    }


    scenarios.take(2).foreach { group =>
      group.name - {
        for {
          (name, scenario) <- group.scenarios
          if options.isScenarioIncluded(name)
        } {
          name in new BlockChainTestSuiteSetup {
            log.debug(s"Running test: ${group.name}/$name")
            runScenario(scenario, env)
          }
        }
      }
    }

    runTests(testName, args)
  }


  private def runScenario(scenario: testBlockChainScenario, env: Env): Unit = {
    val preWorldState = geWorldState(scenario.pre, env.emptyWorld)
    val getPostWorldState = geWorldState(scenario.postState, env.emptyWorld)
    val genesisHeader = scenario.genesisBlockHeader
    val genesisRlp = scenario.genesisRLP
    loadGenesis(genesisHeader, genesisRlp, env)

    val s = env.dataSource.blockHeadersStorage

    1 shouldEqual 1
  }


  private def extractBytes(input: String): Either[JsonRpcError, ByteString] =
    Try(ByteString(decode(input))).toEither.left.map(_ => InvalidParams())

  private def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    Hex.decode(normalized)
  }

  trait BlockChainTestSuiteSetup extends EphemBlockchainTestSetup {
    val emptyWorld = blockchain.getWorldStateProxy(-1, UInt256.Zero, None)
    val blockchainConfig = BlockchainConfig(Config.config)
    val validators = new Validators {
      val blockValidator: BlockValidator = BlockValidator
      val blockHeaderValidator: BlockHeaderValidator = new BlockHeaderValidatorImpl(blockchainConfig)
      val ommersValidator: OmmersValidator = new OmmersValidatorImpl(blockchainConfig)
      val signedTransactionValidator: SignedTransactionValidator = new SignedTransactionValidatorImpl(blockchainConfig)
    }
    val ledger = new LedgerImpl(VM, blockchainConfig)

    val env = Env(emptyWorld, blockchain, validators, ledger, storagesInstance.storages)
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


  def loadGenesis(genesisTestHeader: testBlockHeader, genesisRLP: Option[ByteString], env: Env): Unit = {
    val genesisHeader = testHeaderToHeader(genesisTestHeader)
    env.blockchain.save(Block(genesisHeader, BlockBody(Nil, Nil)))
    env.blockchain.save(genesisHeader.hash, Nil)
    env.blockchain.save(genesisHeader.hash, genesisHeader.difficulty)

//    case Some(_)=>
//    Failure(new RuntimeException("Genesis data present in the database does not match genesis block from file." +
//      " Use different directory for running private blockchains."))
//    case None =>
//      // using empty namespace because ephemDataSource.storage already has the namespace-prefixed keys
//      ephemDataSource.storage.toSeq.grouped(dbConfig.batchSize).foreach(toStore => dataSource.update(IndexedSeq(), Nil, toStore))
//      blockchain.save(Block(header, BlockBody(Nil, Nil)))
//      blockchain.save(header.hash, Nil)
//      blockchain.save(header.hash, header.difficulty)
//      Success(())
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
}



