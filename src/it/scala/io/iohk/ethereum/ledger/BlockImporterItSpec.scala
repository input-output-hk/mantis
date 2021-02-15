package io.iohk.ethereum.ledger

import akka.testkit.TestProbe
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.regular.{BlockFetcher, BlockImporter}
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.Fixtures
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

  val bl = BlockchainImpl(storagesInstance.storages)

  val blockQueue = BlockQueue(bl, SyncConfig(Config.config))

  val genesis = Block(
    Fixtures.Blocks.Genesis.header.copy(stateRoot = ByteString(MerklePatriciaTrie.EmptyRootHash)),
    Fixtures.Blocks.Genesis.body
  )
  val genesisWeight = ChainWeight.zero.increase(genesis.header)

  bl.save(genesis, Seq(), genesisWeight, saveAsBestBlock = true)

  lazy val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator

  val fetcherProbe = TestProbe()
  val ommersPoolProbe = TestProbe()
  val broadcasterProbe = TestProbe()
  val pendingTransactionsManagerProbe = TestProbe()
  val supervisor = TestProbe()

  override lazy val ledger: TestLedgerImpl =  new TestLedgerImpl(validators) {
    override private[ledger] lazy val blockExecution = mock[BlockExecution]
  }

  val blockImporter = system.actorOf(
    BlockImporter.props(
      fetcherProbe.ref,
      ledger,
      bl,
      syncConfig,
      ommersPoolProbe.ref,
      broadcasterProbe.ref,
      pendingTransactionsManagerProbe.ref,
      checkpointBlockGenerator,
      supervisor.ref
    ))

  "BlockImporter" should "return a correct new best block after reorganising longer chain to a shorter one" in {

    val genesis = bl.getBestBlock()
    val block1: Block = getBlock(genesis.number + 1, parent = genesis.header.hash)
    // new chain is shorter but has a higher weight
    val newBlock2: Block = getBlock(genesis.number + 2, difficulty = 101, parent = block1.header.hash)
    val newBlock3: Block = getBlock(genesis.number + 3, difficulty = 333, parent = newBlock2.header.hash)
    val oldBlock2: Block = getBlock(genesis.number + 2, difficulty = 102, parent = block1.header.hash)
    val oldBlock3: Block = getBlock(genesis.number + 3, difficulty = 103, parent = oldBlock2.header.hash)
    val oldBlock4: Block = getBlock(genesis.number + 4, difficulty = 104, parent = oldBlock3.header.hash)

    val weight1 = ChainWeight.totalDifficultyOnly(block1.header.difficulty + 999)
    val newWeight2 = weight1.increase(newBlock2.header)
    val newWeight3 = newWeight2.increase(newBlock3.header)
    val oldWeight2 = weight1.increase(oldBlock2.header)
    val oldWeight3 = oldWeight2.increase(oldBlock3.header)
    val oldWeight4 = oldWeight3.increase(oldBlock4.header)

    //saving initial main chain
    bl.save(block1, Nil, weight1, saveAsBestBlock = true)
    bl.save(oldBlock2, Nil, oldWeight2, saveAsBestBlock = true)
    bl.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)
    bl.save(oldBlock4, Nil, oldWeight4, saveAsBestBlock = true)

    val blockData2 = BlockData(newBlock2, Seq.empty[Receipt], newWeight2)
    val blockData3 = BlockData(newBlock3, Seq.empty[Receipt], newWeight3)

    val newBranch = List(newBlock2, newBlock3)

    blockImporter ! BlockImporter.Start
    blockImporter ! BlockFetcher.PickedBlocks(NonEmptyList.fromListUnsafe(newBranch))

    (ledger.blockExecution.executeAndValidateBlocks _)
      .expects(newBranch, *)
      .returning((List(blockData2, blockData3), None))

    // Saving new blocks, because it's part of executeBlocks method mechanism
    bl.save(blockData2.block, blockData2.receipts, blockData2.weight, saveAsBestBlock = true)
    bl.save(blockData3.block, blockData3.receipts, blockData3.weight, saveAsBestBlock = true)

    bl.getBestBlock() shouldEqual newBlock3
  }
}
