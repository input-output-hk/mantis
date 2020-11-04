package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.validators.BlockHeaderError.{HeaderDifficultyError, HeaderParentNotFoundError}
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.language.postfixOps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BlockImportSpec extends AnyFlatSpec with Matchers with ScalaFutures {

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = scaled(2 seconds), interval = scaled(1 second))

  "Importing blocks" should "ignore existing block" in new ImportBlockTestSetup {
    val block1: Block = getBlock()
    val block2: Block = getBlock()

    setBlockExists(block1, inChain = true, inQueue = false)
    setBestBlock(bestBlock)

    whenReady(ledger.importBlock(block1)) { _ shouldEqual DuplicateBlock }

    setBlockExists(block2, inChain = false, inQueue = true)
    setBestBlock(bestBlock)

    whenReady(ledger.importBlock(block2)) { _ shouldEqual DuplicateBlock }
  }

  it should "import a block to top of the main chain" in new ImportBlockTestSetup {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)
    val difficulty: BigInt = block.header.difficulty
    val hash: ByteString = block.header.hash

    setBlockExists(block, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)

    val newTd: BigInt = currentTd + difficulty
    val blockData = BlockData(block, Seq.empty[Receipt], newTd)
    val emptyWorld: InMemoryWorldStateProxy = BlockchainImpl(storagesInstance.storages)
      .getWorldStateProxy(-1, UInt256.Zero, None, noEmptyAccounts = false, ethCompatibleStorage = true)

    // Just to bypass metrics needs
    (blockchain.getBlockByHash _).expects(*).returning(None)

    (blockQueue.enqueueBlock _).expects(block, bestNum).returning(Some(Leaf(hash, newTd)))
    (blockQueue.getBranch _).expects(hash, true).returning(List(block))

    (blockchain.getBlockHeaderByHash _).expects(*).returning(Some(block.header))
    (blockchain.getWorldStateProxy _).expects(*, *, *, *, *).returning(emptyWorld)

    expectBlockSaved(block, Seq.empty[Receipt], newTd, saveAsBestBlock = true)

    whenReady(ledgerNotFailingAfterExecValidation.importBlock(block)) {
      _ shouldEqual BlockImportedToTop(List(blockData))
    }
  }

  it should "handle exec error when importing to top" in new ImportBlockTestSetup {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)

    val hash: ByteString = block.header.hash
    (blockQueue.enqueueBlock _).expects(block, bestNum).returning(Some(Leaf(hash, currentTd + block.header.difficulty)))
    (blockQueue.getBranch _).expects(hash, true).returning(List(block))

    val emptyWorld: InMemoryWorldStateProxy = BlockchainImpl(storagesInstance.storages)
      .getWorldStateProxy(-1, UInt256.Zero, None, noEmptyAccounts = false, ethCompatibleStorage = true)

    (blockchain.getBlockHeaderByHash _).expects(*).returning(Some(block.header))
    (blockchain.getWorldStateProxy _).expects(*, *, *, *, *).returning(emptyWorld)
    (blockQueue.removeSubtree _).expects(hash)

    whenReady(ledger.importBlock(block)) { _ shouldBe a[BlockImportFailed] }
  }

  // scalastyle:off magic.number
  it should "reorganise chain when a newly enqueued block forms a better branch" in new EphemBlockchain {
    val block1: Block = getBlock(bestNum - 2)
    val newBlock2: Block = getBlock(bestNum - 1, difficulty = 101, parent = block1.header.hash)
    val newBlock3: Block = getBlock(bestNum, difficulty = 105, parent = newBlock2.header.hash)
    val oldBlock2: Block = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)
    val oldBlock3: Block = getBlock(bestNum, difficulty = 103, parent = oldBlock2.header.hash)

    val td1: BigInt = block1.header.difficulty + 999
    val newTd2: BigInt = td1 + newBlock2.header.difficulty
    val newTd3: BigInt = newTd2 + newBlock3.header.difficulty
    val oldTd2: BigInt = td1 + oldBlock2.header.difficulty
    val oldTd3: BigInt = oldTd2 + oldBlock3.header.difficulty

    blockchain.save(block1, Nil, td1, saveAsBestBlock = true)
    blockchain.save(oldBlock2, receipts, oldTd2, saveAsBestBlock = true)
    blockchain.save(oldBlock3, Nil, oldTd3, saveAsBestBlock = true)

    val ancestorForValidation: Block = getBlock(0, difficulty = 1)
    blockchain.save(ancestorForValidation, Nil, 1, saveAsBestBlock = false)

    val oldBranch = List(oldBlock2, oldBlock3)
    val newBranch = List(newBlock2, newBlock3)
    val blockData2 = BlockData(newBlock2, Seq.empty[Receipt], newTd2)
    val blockData3 = BlockData(newBlock3, Seq.empty[Receipt], newTd3)

    (ledgerWithMockedBlockExecution.blockExecution.executeAndValidateBlocks _)
      .expects(newBranch, *)
      .returning((List(blockData2, blockData3), None))

    whenReady(ledgerWithMockedBlockExecution.importBlock(newBlock3)) { result => result shouldEqual BlockEnqueued }
    whenReady(ledgerWithMockedBlockExecution.importBlock(newBlock2)) { result =>
      result shouldEqual ChainReorganised(oldBranch, newBranch, List(newTd2, newTd3))
    }

    // Saving new blocks, because it's part of executeBlocks method mechanism
    blockchain.save(blockData2.block, blockData2.receipts, blockData2.td, saveAsBestBlock = true)
    blockchain.save(blockData3.block, blockData3.receipts, blockData3.td, saveAsBestBlock = true)

    blockchain.getBestBlock() shouldEqual newBlock3
    blockchain.getTotalDifficultyByHash(newBlock3.header.hash) shouldEqual Some(newTd3)

    blockQueue.isQueued(oldBlock2.header.hash) shouldBe true
    blockQueue.isQueued(oldBlock3.header.hash) shouldBe true
  }

  it should "handle error when trying to reorganise chain" in new EphemBlockchain {
    val block1: Block = getBlock(bestNum - 2)
    val newBlock2: Block = getBlock(bestNum - 1, difficulty = 101, parent = block1.header.hash)
    val newBlock3: Block = getBlock(bestNum, difficulty = 105, parent = newBlock2.header.hash)
    val oldBlock2: Block = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)
    val oldBlock3: Block = getBlock(bestNum, difficulty = 103, parent = oldBlock2.header.hash)

    val td1: BigInt = block1.header.difficulty + 999
    val newTd2: BigInt = td1 + newBlock2.header.difficulty
    val newTd3: BigInt = newTd2 + newBlock3.header.difficulty
    val oldTd2: BigInt = td1 + oldBlock2.header.difficulty
    val oldTd3: BigInt = oldTd2 + oldBlock3.header.difficulty

    blockchain.save(block1, Nil, td1, saveAsBestBlock = true)
    blockchain.save(oldBlock2, receipts, oldTd2, saveAsBestBlock = true)
    blockchain.save(oldBlock3, Nil, oldTd3, saveAsBestBlock = true)

    val ancestorForValidation: Block = getBlock(0, difficulty = 1)
    blockchain.save(ancestorForValidation, Nil, 1, saveAsBestBlock = false)

    val oldBranch = List(oldBlock2, oldBlock3)
    val newBranch = List(newBlock2, newBlock3)
    val blockData2 = BlockData(newBlock2, Seq.empty[Receipt], newTd2)
    val blockData3 = BlockData(newBlock3, Seq.empty[Receipt], newTd3)

    (ledgerWithMockedBlockExecution.blockExecution.executeAndValidateBlocks _)
      .expects(newBranch, *)
      .returning((List(blockData2), Some(execError)))

    whenReady(ledgerWithMockedBlockExecution.importBlock(newBlock3)) { _ shouldEqual BlockEnqueued }
    whenReady(ledgerWithMockedBlockExecution.importBlock(newBlock2)) { _ shouldBe a[BlockImportFailed] }

    blockchain.getBestBlock() shouldEqual oldBlock3
    blockchain.getTotalDifficultyByHash(oldBlock3.header.hash) shouldEqual Some(oldTd3)

    blockQueue.isQueued(newBlock2.header.hash) shouldBe true
    blockQueue.isQueued(newBlock3.header.hash) shouldBe false
  }

  it should "report an orphaned block" in new ImportBlockTestSetup {
    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }

    val newConsensus: TestConsensus = consensus.withValidators(validators).withVM(new Mocks.MockVM())
    val ledgerWithMockedValidators =
      new LedgerImpl(blockchain, blockQueue, blockchainConfig, newConsensus, testContext)

    val newBlock: Block = getBlock(number = bestNum + 1)
    setBlockExists(newBlock, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)

    (validators.blockHeaderValidator
      .validate(_: BlockHeader, _: GetBlockHeaderByHash))
      .expects(newBlock.header, *)
      .returning(Left(HeaderParentNotFoundError))

    whenReady(ledgerWithMockedValidators.importBlock(newBlock)) { _ shouldEqual UnknownParent }
  }

  it should "validate blocks prior to import" in new ImportBlockTestSetup {
    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }

    val newConsensus: TestConsensus = consensus.withValidators(validators).withVM(new Mocks.MockVM())
    val ledgerWithMockedValidators =
      new LedgerImpl(blockchain, blockQueue, blockchainConfig, newConsensus, testContext)

    val newBlock: Block = getBlock(number = bestNum + 1)
    setBlockExists(newBlock, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)

    (validators.blockHeaderValidator
      .validate(_: BlockHeader, _: GetBlockHeaderByHash))
      .expects(newBlock.header, *)
      .returning(Left(HeaderDifficultyError))

    whenReady(ledgerWithMockedValidators.importBlock(newBlock)) {
      _ shouldEqual BlockImportFailed(HeaderDifficultyError.toString)
    }
  }

  it should "correctly handle importing genesis block" in new ImportBlockTestSetup {
    val genesisBlock = Block(genesisHeader, BlockBody.empty)

    setBestBlock(genesisBlock)
    setBlockExists(genesisBlock, inChain = true, inQueue = true)

    whenReady(failLedger.importBlock(genesisBlock)) { _ shouldEqual DuplicateBlock }
  }

  it should "correctly import block with ommers and ancestor in block queue " in new OmmersTestSetup {
    val ancestorForValidation: Block = getBlock(0, difficulty = 1)
    val ancestorForValidation1: Block = getBlock(difficulty = 2, parent = ancestorForValidation.header.hash)
    val ancestorForValidation2: Block = getBlock(2, difficulty = 3, parent = ancestorForValidation1.header.hash)

    val block1: Block = getBlock(bestNum - 2, parent = ancestorForValidation2.header.hash)
    val ommerBlock: Block = getBlock(bestNum - 1, difficulty = 101, parent = block1.header.hash)
    val oldBlock2: Block = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)
    val oldBlock3: Block = getBlock(bestNum, difficulty = 103, parent = oldBlock2.header.hash)
    val newBlock2: Block = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)

    val newBlock3WithOmmer: Block =
      getBlock(bestNum, difficulty = 105, parent = newBlock2.header.hash, ommers = Seq(ommerBlock.header))

    val td1: BigInt = block1.header.difficulty + 999
    val oldTd2: BigInt = td1 + oldBlock2.header.difficulty
    val oldTd3: BigInt = oldTd2 + oldBlock3.header.difficulty

    val newTd2: BigInt = td1 + newBlock2.header.difficulty
    val newTd3: BigInt = newTd2 + newBlock3WithOmmer.header.difficulty

    blockchain.save(ancestorForValidation, Nil, 1, saveAsBestBlock = false)
    blockchain.save(ancestorForValidation1, Nil, 3, saveAsBestBlock = false)
    blockchain.save(ancestorForValidation2, Nil, 6, saveAsBestBlock = false)

    blockchain.save(block1, Nil, td1, saveAsBestBlock = true)
    blockchain.save(oldBlock2, receipts, oldTd2, saveAsBestBlock = true)
    blockchain.save(oldBlock3, Nil, oldTd3, saveAsBestBlock = true)

    val oldBranch = List(oldBlock2, oldBlock3)
    val newBranch = List(newBlock2, newBlock3WithOmmer)
    val blockData2 = BlockData(newBlock2, Seq.empty[Receipt], newTd2)
    val blockData3 = BlockData(newBlock3WithOmmer, Seq.empty[Receipt], newTd3)

    (ledgerWithMockedBlockExecution.blockExecution.executeAndValidateBlocks _)
      .expects(newBranch, *)
      .returning((List(blockData2, blockData3), None))

    whenReady(ledgerWithMockedBlockExecution.importBlock(newBlock2)) { _ shouldEqual BlockEnqueued }
    whenReady(ledgerWithMockedBlockExecution.importBlock(newBlock3WithOmmer)) { result =>
      result shouldEqual ChainReorganised(oldBranch, newBranch, List(newTd2, newTd3))
    }

    // Saving new blocks, because it's part of executeBlocks method mechanism
    blockchain.save(blockData2.block, blockData2.receipts, blockData2.td, saveAsBestBlock = true)
    blockchain.save(blockData3.block, blockData3.receipts, blockData3.td, saveAsBestBlock = true)

    blockchain.getBestBlock() shouldEqual newBlock3WithOmmer
  }

  it should "correctly import a checkpoint block" in new EphemBlockchain with CheckpointHelpers {
    val parentBlock: Block = getBlock(bestNum)
    val regularBlock: Block = getBlock(bestNum + 1, difficulty = 200, parent = parentBlock.hash)
    val checkpointBlock: Block = getCheckpointBlock(parentBlock, difficulty = 100)

    val tdParent = parentBlock.header.difficulty + 999
    val tdRegular = tdParent + regularBlock.header.difficulty
    val tdCheckpoint = tdParent + checkpointBlock.header.difficulty

    blockchain.save(parentBlock, Nil, tdParent, saveAsBestBlock = true)
    blockchain.save(regularBlock, Nil, tdRegular, saveAsBestBlock = true)

    (ledgerWithMockedBlockExecution.blockExecution.executeAndValidateBlocks _)
      .expects(List(checkpointBlock), *)
      .returning((List(BlockData(checkpointBlock, Nil, tdCheckpoint)), None))

    whenReady(ledgerWithMockedBlockExecution.importBlock(checkpointBlock)) { result =>
      result shouldEqual ChainReorganised(
        List(regularBlock),
        List(checkpointBlock),
        List(tdCheckpoint)
      )
    }

    // Saving new blocks, because it's part of executeBlocks method mechanism
    blockchain.save(checkpointBlock, Nil, tdCheckpoint, saveAsBestBlock = true)

    blockchain.getBestBlock() shouldEqual checkpointBlock
    blockchain.getTotalDifficultyByHash(checkpointBlock.hash) shouldEqual Some(tdCheckpoint)
  }

  it should "not import a block with higher difficulty that does not follow a checkpoint" in new EphemBlockchain
    with CheckpointHelpers {

    val parentBlock: Block = getBlock(bestNum)
    val regularBlock: Block = getBlock(bestNum + 1, difficulty = 200, parent = parentBlock.hash)
    val checkpointBlock: Block = getCheckpointBlock(parentBlock, difficulty = 100)

    val tdParent = parentBlock.header.difficulty + 999
    val tdCheckpoint = tdParent + checkpointBlock.header.difficulty

    blockchain.save(parentBlock, Nil, tdParent, saveAsBestBlock = true)
    blockchain.save(checkpointBlock, Nil, tdCheckpoint, saveAsBestBlock = true)

    whenReady(ledgerWithMockedBlockExecution.importBlock(regularBlock)) { result =>
      result shouldEqual BlockEnqueued
    }

    blockchain.getBestBlock() shouldEqual checkpointBlock
  }

  trait ImportBlockTestSetup extends TestSetupWithVmAndValidators with MockBlockchain

}
