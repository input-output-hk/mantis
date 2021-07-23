package io.iohk.ethereum.ledger

import akka.actor.ActorRef
import akka.testkit.TestProbe
import akka.util.ByteString

import cats.data.NonEmptyList

import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService

import scala.concurrent.duration._

import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.Timeouts
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.NewCheckpoint
import io.iohk.ethereum.checkpointing.CheckpointingTestHelpers
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.pow.validators.OmmersValidator
import io.iohk.ethereum.consensus.pow.validators.StdOmmersValidator
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.SyncConfig

class BlockImporterItSpec
    extends MockFactory
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with Eventually
    with NormalPatience {

  implicit val testScheduler: SchedulerService = Scheduler.fixedPool("test", 32)

  override def afterAll(): Unit = {
    testScheduler.shutdown()
    testScheduler.awaitTermination(60.second)
  }

  "BlockImporter" should "not discard blocks of the main chain if the reorganisation failed" in new TestFixture() {

    override val blockImporter = system.actorOf(
      BlockImporter.props(
        fetcherProbe.ref,
        mkBlockImport(validators = successValidators),
        blockchain,
        blockchainReader,
        storagesInstance.storages.stateStorage,
        new BranchResolution(blockchain, blockchainReader),
        syncConfig,
        ommersPoolProbe.ref,
        broadcasterProbe.ref,
        pendingTransactionsManagerProbe.ref,
        supervisor.ref,
        this
      )
    )

    blockImporter ! BlockImporter.Start
    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    //because the blocks are not valid, we shouldn't reorganise, but at least stay with a current chain, and the best block of the current chain is oldBlock4
    eventually(blockchainReader.getBestBlock().get shouldEqual oldBlock4)
  }

  it should "return a correct new best block after reorganising longer chain to a shorter one if its weight is bigger" in new StartedImportFixture() {
    //returning discarded initial chain
    blockchainWriter.save(oldBlock2, Nil, oldWeight2, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock4, Nil, oldWeight4, saveAsBestBlock = true)

    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    eventually(blockchainReader.getBestBlock().get shouldEqual newBlock3)
  }

  it should "return Unknown branch, in case of PickedBlocks with block that has a parent that's not in the chain" in new StartedImportFixture() {
    val newBlock4ParentOldBlock3: Block =
      getBlock(genesisBlock.number + 4, difficulty = 104, parent = oldBlock3.header.hash)
    val newBlock4WeightParentOldBlock3 = oldWeight3.increase(newBlock4ParentOldBlock3.header)

    //Block n5 with oldBlock4 as parent
    val newBlock5ParentOldBlock4: Block =
      getBlock(
        genesisBlock.number + 5,
        difficulty = 108,
        parent = oldBlock4.header.hash,
        ommers = Seq(oldBlock4.header)
      )

    blockchainWriter.save(oldBlock2, Nil, oldWeight2, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock4, Nil, oldWeight4, saveAsBestBlock = true)
    // simulation of node restart
    blockchain.saveBestKnownBlocks(oldBlock3.header.hash, oldBlock3.number)
    blockchainWriter.save(newBlock4ParentOldBlock3, Nil, newBlock4WeightParentOldBlock3, saveAsBestBlock = true)

    //not reorganising anymore until oldBlock4(not part of the chain anymore), no block/ommer validation when not part of the chain, resolveBranch is returning UnknownBranch
    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(List(newBlock5ParentOldBlock4)))

    eventually(blockchainReader.getBestBlock().get shouldEqual newBlock4ParentOldBlock3)
  }

  it should "switch to a branch with a checkpoint" in new StartedImportFixture() {

    val checkpoint = ObjectGenerators.fakeCheckpointGen(3, 3).sample.get
    val oldBlock5WithCheckpoint: Block = checkpointBlockGenerator.generate(oldBlock4, checkpoint)
    blockchainWriter.save(oldBlock5WithCheckpoint, Nil, oldWeight4, saveAsBestBlock = true)

    override val newBranch = List(newBlock2, newBlock3)

    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    eventually(blockchainReader.getBestBlock().get shouldEqual oldBlock5WithCheckpoint)
    eventually(blockchain.getLatestCheckpointBlockNumber() shouldEqual oldBlock5WithCheckpoint.header.number)
  }

  it should "switch to a branch with a newer checkpoint" in new StartedImportFixture() {

    val checkpoint = ObjectGenerators.fakeCheckpointGen(3, 3).sample.get
    val newBlock4WithCheckpoint: Block = checkpointBlockGenerator.generate(newBlock3, checkpoint)
    blockchainWriter.save(newBlock4WithCheckpoint, Nil, newWeight3, saveAsBestBlock = true)

    override val newBranch = List(newBlock4WithCheckpoint)

    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    eventually(blockchainReader.getBestBlock().get shouldEqual newBlock4WithCheckpoint)
    eventually(blockchain.getLatestCheckpointBlockNumber() shouldEqual newBlock4WithCheckpoint.header.number)
  }

  it should "return a correct checkpointed block after receiving a request for generating a new checkpoint" in new StartedImportFixture() {

    val parent = blockchainReader.getBestBlock().get
    val newBlock5: Block = getBlock(genesisBlock.number + 5, difficulty = 104, parent = parent.header.hash)
    val newWeight5 = newWeight3.increase(newBlock5.header)

    blockchainWriter.save(newBlock5, Nil, newWeight5, saveAsBestBlock = true)

    val signatures = CheckpointingTestHelpers.createCheckpointSignatures(
      Seq(crypto.generateKeyPair(secureRandom)),
      newBlock5.hash
    )
    val checkpointBlock = checkpointBlockGenerator.generate(newBlock5, Checkpoint(signatures))
    blockImporter ! NewCheckpoint(checkpointBlock)

    eventually(blockchainReader.getBestBlock().get shouldEqual checkpointBlock)
    eventually(blockchain.getLatestCheckpointBlockNumber() shouldEqual newBlock5.header.number + 1)
  }

  it should "ask BlockFetcher to resolve missing node" in new TestFixture() {
    val parent = blockchainReader.getBestBlock().get
    val newBlock: Block = getBlock(genesisBlock.number + 5, difficulty = 104, parent = parent.header.hash)
    val invalidBlock = newBlock.copy(header = newBlock.header.copy(beneficiary = Address(111).bytes))

    override val blockImporter = system.actorOf(
      BlockImporter.props(
        fetcherProbe.ref,
        mkBlockImport(validators = successValidators),
        blockchain,
        blockchainReader,
        storagesInstance.storages.stateStorage,
        new BranchResolution(blockchain, blockchainReader),
        syncConfig,
        ommersPoolProbe.ref,
        broadcasterProbe.ref,
        pendingTransactionsManagerProbe.ref,
        supervisor.ref,
        this
      )
    )

    blockImporter ! BlockImporter.Start
    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(List(invalidBlock)))

    eventually {
      val msg = fetcherProbe
        .fishForMessage(Timeouts.normalTimeout) {
          case BlockFetcher.FetchStateNode(_, _) => true
          case _                                 => false
        }
        .asInstanceOf[BlockFetcher.FetchStateNode]

      msg.hash.length should be > 0
    }

  }
}

class TestFixture extends TestSetupWithVmAndValidators {

  override lazy val blockQueue: BlockQueue = BlockQueue(blockchain, blockchainReader, SyncConfig(Config.config))

  val genesis: Block = Block(
    Fixtures.Blocks.Genesis.header.copy(stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash)),
    Fixtures.Blocks.Genesis.body
  )
  val genesisWeight: ChainWeight = ChainWeight.zero.increase(genesis.header)

  blockchainWriter.save(genesis, Seq(), genesisWeight, saveAsBestBlock = true)

  lazy val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator

  val fetcherProbe: TestProbe = TestProbe()
  val ommersPoolProbe: TestProbe = TestProbe()
  val broadcasterProbe: TestProbe = TestProbe()
  val pendingTransactionsManagerProbe: TestProbe = TestProbe()
  val supervisor: TestProbe = TestProbe()

  val emptyWorld: InMemoryWorldStateProxy = InMemoryWorldStateProxy(
    storagesInstance.storages.evmCodeStorage,
    blockchain.getBackingMptStorage(-1),
    (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
    blockchainConfig.accountStartNonce,
    ByteString(MerklePatriciaTrie.EmptyRootHash),
    noEmptyAccounts = false,
    ethCompatibleStorage = true
  )

  override protected lazy val successValidators: Validators = new Mocks.MockValidatorsAlwaysSucceed {
    override val ommersValidator: OmmersValidator = new StdOmmersValidator(blockHeaderValidator)
  }

  override lazy val blockImport: BlockImport = mkBlockImport(
    validators = successValidators,
    blockExecutionOpt = Some(
      new BlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.evmCodeStorage,
        mining.blockPreparator,
        new BlockValidation(mining, blockchainReader, blockQueue)
      ) {
        override def executeAndValidateBlock(
            block: Block,
            alreadyValidated: Boolean = false
        )(implicit blockchainConfig: BlockchainConfig): Either[BlockExecutionError, Seq[Receipt]] =
          Right(BlockResult(emptyWorld).receipts)
      }
    )
  )

  val blockImporter: ActorRef = system.actorOf(
    BlockImporter.props(
      fetcherProbe.ref,
      blockImport,
      blockchain,
      blockchainReader,
      storagesInstance.storages.stateStorage,
      new BranchResolution(blockchain, blockchainReader),
      syncConfig,
      ommersPoolProbe.ref,
      broadcasterProbe.ref,
      pendingTransactionsManagerProbe.ref,
      supervisor.ref,
      this
    )
  )

  val genesisBlock = blockchainReader.genesisBlock
  val block1: Block = getBlock(genesisBlock.number + 1, parent = genesisBlock.header.hash)
  // new chain is shorter but has a higher weight
  val newBlock2: Block = getBlock(genesisBlock.number + 2, difficulty = 108, parent = block1.header.hash)
  val newBlock3: Block = getBlock(genesisBlock.number + 3, difficulty = 300, parent = newBlock2.header.hash)
  val oldBlock2: Block = getBlock(genesisBlock.number + 2, difficulty = 102, parent = block1.header.hash)
  val oldBlock3: Block = getBlock(genesisBlock.number + 3, difficulty = 103, parent = oldBlock2.header.hash)
  val oldBlock4: Block = getBlock(genesisBlock.number + 4, difficulty = 104, parent = oldBlock3.header.hash)

  val weight1: ChainWeight = ChainWeight.totalDifficultyOnly(block1.header.difficulty)
  val newWeight2: ChainWeight = weight1.increase(newBlock2.header)
  val newWeight3: ChainWeight = newWeight2.increase(newBlock3.header)
  val oldWeight2: ChainWeight = weight1.increase(oldBlock2.header)
  val oldWeight3: ChainWeight = oldWeight2.increase(oldBlock3.header)
  val oldWeight4: ChainWeight = oldWeight3.increase(oldBlock4.header)

  //saving initial main chain
  blockchainWriter.save(block1, Nil, weight1, saveAsBestBlock = true)
  blockchainWriter.save(oldBlock2, Nil, oldWeight2, saveAsBestBlock = true)
  blockchainWriter.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)
  blockchainWriter.save(oldBlock4, Nil, oldWeight4, saveAsBestBlock = true)

  val oldBranch: List[Block] = List(oldBlock2, oldBlock3, oldBlock4)
  val newBranch: List[Block] = List(newBlock2, newBlock3)
}

class StartedImportFixture extends TestFixture {
  blockImporter ! BlockImporter.Start
}
