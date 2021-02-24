package io.iohk.ethereum.ledger

import akka.testkit.TestProbe
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.NewCheckpoint
import io.iohk.ethereum.blockchain.sync.regular.{BlockFetcher, BlockImporter}
import io.iohk.ethereum.checkpointing.CheckpointingTestHelpers
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.{Fixtures, ObjectGenerators, crypto}
import io.iohk.ethereum.ledger.Ledger.BlockResult
import monix.execution.Scheduler
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class BlockImporterItSpec extends MockFactory with TestSetupWithVmAndValidators with AsyncFlatSpecLike with Matchers with BeforeAndAfterAll {

  implicit val testScheduler = Scheduler.fixedPool("test", 32)

  override def afterAll(): Unit = {
    testScheduler.shutdown()
    testScheduler.awaitTermination(60.second)
  }

  val blockQueue = BlockQueue(blockchain, SyncConfig(Config.config))

  val genesis = Block(
    Fixtures.Blocks.Genesis.header.copy(stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash)),
    Fixtures.Blocks.Genesis.body
  )
  val genesisWeight = ChainWeight.zero.increase(genesis.header)

  blockchain.save(genesis, Seq(), genesisWeight, saveAsBestBlock = true)

  lazy val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator

  val fetcherProbe = TestProbe()
  val ommersPoolProbe = TestProbe()
  val broadcasterProbe = TestProbe()
  val pendingTransactionsManagerProbe = TestProbe()
  val supervisor = TestProbe()

  val emptyWorld: InMemoryWorldStateProxy =
    blockchain.getWorldStateProxy(
      -1,
      UInt256.Zero,
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      noEmptyAccounts = false,
      ethCompatibleStorage = true
    )

  override lazy val ledger = new TestLedgerImpl(successValidators)  {
    override private[ledger] lazy val blockExecution = new BlockExecution(blockchain, blockchainConfig, consensus.blockPreparator, blockValidation) {
      override def executeAndValidateBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]] =
        Right(BlockResult(emptyWorld).receipts)
    }
  }

  val blockImporter = system.actorOf(
    BlockImporter.props(
      fetcherProbe.ref,
      ledger,
      blockchain,
      syncConfig,
      ommersPoolProbe.ref,
      broadcasterProbe.ref,
      pendingTransactionsManagerProbe.ref,
      checkpointBlockGenerator,
      supervisor.ref
    ))

  val genesisBlock = blockchain.genesisBlock
  val block1: Block = getBlock(genesisBlock.number + 1, parent = genesisBlock.header.hash)
  // new chain is shorter but has a higher weight
  val newBlock2: Block = getBlock(genesisBlock.number + 2, difficulty = 108, parent = block1.header.hash)
  val newBlock3: Block = getBlock(genesisBlock.number + 3, difficulty = 300, parent = newBlock2.header.hash)
  val oldBlock2: Block = getBlock(genesisBlock.number + 2, difficulty = 102, parent = block1.header.hash)
  val oldBlock3: Block = getBlock(genesisBlock.number + 3, difficulty = 103, parent = oldBlock2.header.hash)
  val oldBlock4: Block = getBlock(genesisBlock.number + 4, difficulty = 104, parent = oldBlock3.header.hash)

  val weight1 = ChainWeight.totalDifficultyOnly(block1.header.difficulty)
  val newWeight2 = weight1.increase(newBlock2.header)
  val newWeight3 = newWeight2.increase(newBlock3.header)
  val oldWeight2 = weight1.increase(oldBlock2.header)
  val oldWeight3 = oldWeight2.increase(oldBlock3.header)
  val oldWeight4 = oldWeight3.increase(oldBlock4.header)

  //saving initial main chain
  blockchain.save(block1, Nil, weight1, saveAsBestBlock = true)
  blockchain.save(oldBlock2, Nil, oldWeight2, saveAsBestBlock = true)
  blockchain.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)
  blockchain.save(oldBlock4, Nil, oldWeight4, saveAsBestBlock = true)

  val oldBranch = List(oldBlock2, oldBlock3, oldBlock4)
  val newBranch = List(newBlock2, newBlock3)

  blockImporter ! BlockImporter.Start

  "BlockImporter" should "not discard blocks of the main chain if the reorganisation failed" in {

    //ledger with not mocked blockExecution
    val ledger = new TestLedgerImpl(successValidators)
    val blockImporter = system.actorOf(
      BlockImporter.props(
        fetcherProbe.ref,
        ledger,
        blockchain,
        syncConfig,
        ommersPoolProbe.ref,
        broadcasterProbe.ref,
        pendingTransactionsManagerProbe.ref,
        checkpointBlockGenerator,
        supervisor.ref
      ))

    blockImporter ! BlockImporter.Start
    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    Thread.sleep(1000)
    //because the blocks are not valid, we shouldn't reorganise, but at least stay with a current chain, and the best block of the current chain is oldBlock4
    blockchain.getBestBlock().get shouldEqual oldBlock4
  }

  it should "return a correct new best block after reorganising longer chain to a shorter one" in {

    //returning discarded initial chain
    blockchain.save(oldBlock2, Nil, oldWeight2, saveAsBestBlock = true)
    blockchain.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)
    blockchain.save(oldBlock4, Nil, oldWeight4, saveAsBestBlock = true)

    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    Thread.sleep(200)
    blockchain.getBestBlock().get shouldEqual newBlock3
  }


  it should "switch to a branch with a checkpoint" in {

    val chackpoint = ObjectGenerators.fakeCheckpointGen(3, 3).sample.get
    val oldBlock5WithCheckpoint: Block = checkpointBlockGenerator.generate(oldBlock4, chackpoint)
    blockchain.save(oldBlock5WithCheckpoint, Nil, oldWeight4, saveAsBestBlock = true)

    val newBranch = List(newBlock2, newBlock3)

    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    Thread.sleep(200)
    blockchain.getBestBlock().get shouldEqual oldBlock5WithCheckpoint
    blockchain.getLatestCheckpointBlockNumber() shouldEqual oldBlock5WithCheckpoint.header.number
  }

  it should "return a correct checkpointed block after reorganising longer chain to a shorter one and back" in {

    val chackpoint = ObjectGenerators.fakeCheckpointGen(3, 3).sample.get
    val newBlock4WithCheckpoint: Block = checkpointBlockGenerator.generate(newBlock3, chackpoint)
    blockchain.save(newBlock4WithCheckpoint, Nil, newWeight3, saveAsBestBlock = true)

    val newBranch = List(newBlock4WithCheckpoint)

    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    Thread.sleep(200)
    blockchain.getBestBlock().get shouldEqual newBlock4WithCheckpoint
    blockchain.getLatestCheckpointBlockNumber() shouldEqual newBlock4WithCheckpoint.header.number
  }

  it should "return a correct checkpointed block after receiving a new chackpoint from morpho" in {

    val parent = blockchain.getBestBlock().get
    val newBlock5: Block = getBlock(genesisBlock.number + 5, difficulty = 104, parent = parent.header.hash)
    val newWeight5 = newWeight3.increase(newBlock5.header)

    blockchain.save(newBlock5, Nil, newWeight5, saveAsBestBlock = true)

    val signatures = CheckpointingTestHelpers.createCheckpointSignatures(
      Seq(crypto.generateKeyPair(secureRandom)),
      newBlock5.hash
    )
    blockImporter ! NewCheckpoint(newBlock5.hash, signatures)

    val checkpointBlock = checkpointBlockGenerator.generate(newBlock5, Checkpoint(signatures))

    Thread.sleep(1000)
    blockchain.getBestBlock().get shouldEqual checkpointBlock
    blockchain.getLatestCheckpointBlockNumber() shouldEqual newBlock5.header.number + 1
  }
}
