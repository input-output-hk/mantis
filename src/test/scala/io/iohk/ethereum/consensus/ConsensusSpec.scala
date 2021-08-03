package io.iohk.ethereum.consensus

import akka.util.ByteString

import scala.concurrent.duration._
import scala.language.postfixOps

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.regular.BlockEnqueued
import io.iohk.ethereum.blockchain.sync.regular.BlockImportFailed
import io.iohk.ethereum.blockchain.sync.regular.BlockImportedToTop
import io.iohk.ethereum.blockchain.sync.regular.ChainReorganised
import io.iohk.ethereum.blockchain.sync.regular.DuplicateBlock
import io.iohk.ethereum.consensus.mining._
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderDifficultyError
import io.iohk.ethereum.consensus.validators.BlockHeaderError.HeaderParentNotFoundError
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockData
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.ledger.CheckpointHelpers
import io.iohk.ethereum.ledger.EphemBlockchain
import io.iohk.ethereum.ledger.MockBlockchain
import io.iohk.ethereum.ledger.OmmersTestSetup
import io.iohk.ethereum.ledger.TestSetupWithVmAndValidators
import io.iohk.ethereum.mpt.LeafNode
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.utils.BlockchainConfig

class ConsensusSpec extends AnyFlatSpec with Matchers with ScalaFutures {

  implicit override val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = scaled(2 seconds), interval = scaled(1 second))

  "Consensus" should "ignore duplicated block" in new ImportBlockTestSetup {
    val block1: Block = getBlock()
    val block2: Block = getBlock()

    setBlockExists(block1, inChain = true, inQueue = false)
    setBestBlock(bestBlock)

    whenReady(consensusAdapter.evaluateBranchBlock(block1).runToFuture)(_ shouldEqual DuplicateBlock)

    setBlockExists(block2, inChain = false, inQueue = true)
    setBestBlock(bestBlock)

    whenReady(consensusAdapter.evaluateBranchBlock(block2).runToFuture)(_ shouldEqual DuplicateBlock)
  }

  it should "import a block to the top of the main chain" in new ImportBlockTestSetup {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)
    val difficulty: BigInt = block.header.difficulty
    val hash: ByteString = block.header.hash

    setBlockExists(block, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setChainWeightForBlock(bestBlock, currentWeight)

    val newWeight = currentWeight.increaseTotalDifficulty(difficulty)
    val blockData = BlockData(block, Seq.empty[Receipt], newWeight)

    // Just to bypass metrics needs
    (blockchainReader.getBlockByHash _).expects(*).returning(None)

    (blockQueue.enqueueBlock _).expects(block, bestNum).returning(Some(Leaf(hash, newWeight)))
    (blockQueue.getBranch _).expects(hash, true).returning(List(block))

    (blockchainReader.getBlockHeaderByHash _).expects(*).returning(Some(block.header))
    (blockchain.getBackingMptStorage _)
      .expects(*)
      .returning(storagesInstance.storages.stateStorage.getBackingStorage(6))

    expectBlockSaved(block, Seq.empty[Receipt], newWeight, saveAsBestBlock = true)
    whenReady(blockImportNotFailingAfterExecValidation.evaluateBranchBlock(block).runToFuture) {
      _ shouldEqual BlockImportedToTop(List(blockData))
    }
  }

  it should "handle exec error when importing to top" in new ImportBlockTestSetup {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setChainWeightForBlock(bestBlock, currentWeight)

    val hash: ByteString = block.header.hash
    (blockQueue.enqueueBlock _)
      .expects(block, bestNum)
      .returning(Some(Leaf(hash, currentWeight.increase(block.header))))
    (blockQueue.getBranch _).expects(hash, true).returning(List(block))

    val mptStorage = mock[MptStorage]
    val mptNode = LeafNode(
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      ByteString(MerklePatriciaTrie.EmptyRootHash),
      Some(MerklePatriciaTrie.EmptyRootHash),
      Some(MerklePatriciaTrie.EmptyRootHash)
    )

    (blockchainReader.getBlockHeaderByHash _).expects(*).returning(Some(block.header))
    (blockchainReader.getBlockHeaderByNumber _).expects(*).returning(Some(block.header))
    (blockchain.getBackingMptStorage _).expects(*).returning(mptStorage)
    (mptStorage.get _).expects(*).returning(mptNode)

    (blockQueue.removeSubtree _).expects(*)

    whenReady(consensusAdapter.evaluateBranchBlock(block).runToFuture)(
      _ shouldBe BlockImportFailed("MPTError(io.iohk.ethereum.mpt.MerklePatriciaTrie$MPTException: Invalid Node)")
    )
  }

  it should "handle no best block available error when importing to top" in new ImportBlockTestSetup {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, inChain = false, inQueue = false)
    (blockchainReader.getBestBlock _).expects().returning(None)
    setChainWeightForBlock(bestBlock, currentWeight)

    whenReady(consensusAdapter.evaluateBranchBlock(block).runToFuture)(
      _ shouldBe BlockImportFailed("Couldn't find the current best block")
    )
  }

  it should "handle total difficulty error when importing to top" in new ImportBlockTestSetup {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    (blockchainReader.getChainWeightByHash _).expects(*).returning(None)

    (blockchainReader.getBlockHeaderByHash _).expects(*).returning(Some(block.header))

    whenReady(consensusAdapter.evaluateBranchBlock(block).runToFuture) { result =>
      result shouldBe a[BlockImportFailed]
      result
        .asInstanceOf[BlockImportFailed]
        .error
        .should(startWith("Couldn't get total difficulty for current best block"))
    }
  }

  // scalastyle:off magic.number
  it should "reorganise chain when a newly enqueued block forms a better branch" in new EphemBlockchain {
    val block1: Block = getBlock(bestNum - 2)
    val newBlock2: Block = getBlock(bestNum - 1, difficulty = 101, parent = block1.header.hash)
    val newBlock3: Block = getBlock(bestNum, difficulty = 105, parent = newBlock2.header.hash)
    val oldBlock2: Block = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)
    val oldBlock3: Block = getBlock(bestNum, difficulty = 103, parent = oldBlock2.header.hash)

    val weight1 = ChainWeight.totalDifficultyOnly(block1.header.difficulty + 999)
    val newWeight2 = weight1.increase(newBlock2.header)
    val newWeight3 = newWeight2.increase(newBlock3.header)
    val oldWeight2 = weight1.increase(oldBlock2.header)
    val oldWeight3 = oldWeight2.increase(oldBlock3.header)

    blockchainWriter.save(block1, Nil, weight1, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock2, receipts, oldWeight2, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)

    val ancestorForValidation: Block = getBlock(0, difficulty = 1)
    blockchainWriter.save(ancestorForValidation, Nil, ChainWeight.totalDifficultyOnly(1), saveAsBestBlock = false)

    val oldBranch = List(oldBlock2, oldBlock3)
    val newBranch = List(newBlock2, newBlock3)
    val blockData2 = BlockData(newBlock2, Seq.empty[Receipt], newWeight2)
    val blockData3 = BlockData(newBlock3, Seq.empty[Receipt], newWeight3)

    val mockExecution = mock[BlockExecution]
    (mockExecution
      .executeAndValidateBlocks(_: List[Block], _: ChainWeight)(_: BlockchainConfig))
      .expects(newBranch, *, *)
      .returning((List(blockData2, blockData3), None))

    val withMockedBlockExecution = blockImportWithMockedBlockExecution(mockExecution)
    whenReady(withMockedBlockExecution.evaluateBranchBlock(newBlock3).runToFuture)(
      _ shouldEqual BlockEnqueued
    )
    whenReady(withMockedBlockExecution.evaluateBranchBlock(newBlock2).runToFuture) { result =>
      result shouldEqual ChainReorganised(oldBranch, newBranch, List(newWeight2, newWeight3))
    }

    // Saving new blocks, because it's part of executeBlocks method mechanism
    blockchainWriter.save(blockData2.block, blockData2.receipts, blockData2.weight, saveAsBestBlock = true)
    blockchainWriter.save(blockData3.block, blockData3.receipts, blockData3.weight, saveAsBestBlock = true)

    blockchainReader.getBestBlock().get shouldEqual newBlock3
    blockchainReader.getChainWeightByHash(newBlock3.header.hash) shouldEqual Some(newWeight3)

    blockQueue.isQueued(oldBlock2.header.hash) shouldBe true
    blockQueue.isQueued(oldBlock3.header.hash) shouldBe true
  }

  it should "handle error when trying to reorganise chain" in new EphemBlockchain {
    val block1: Block = getBlock(bestNum - 2)
    val newBlock2: Block = getBlock(bestNum - 1, difficulty = 101, parent = block1.header.hash)
    val newBlock3: Block = getBlock(bestNum, difficulty = 105, parent = newBlock2.header.hash)
    val oldBlock2: Block = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)
    val oldBlock3: Block = getBlock(bestNum, difficulty = 103, parent = oldBlock2.header.hash)

    val weight1 = ChainWeight.totalDifficultyOnly(block1.header.difficulty + 999)
    val newWeight2 = weight1.increase(newBlock2.header)
    newWeight2.increase(newBlock3.header)
    val oldWeight2 = weight1.increase(oldBlock2.header)
    val oldWeight3 = oldWeight2.increase(oldBlock3.header)

    blockchainWriter.save(block1, Nil, weight1, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock2, receipts, oldWeight2, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)

    val ancestorForValidation: Block = getBlock(0, difficulty = 1)
    blockchainWriter.save(ancestorForValidation, Nil, ChainWeight.totalDifficultyOnly(1), saveAsBestBlock = false)

    val newBranch = List(newBlock2, newBlock3)
    val blockData2 = BlockData(newBlock2, Seq.empty[Receipt], newWeight2)

    val mockExecution = mock[BlockExecution]
    (mockExecution
      .executeAndValidateBlocks(_: List[Block], _: ChainWeight)(_: BlockchainConfig))
      .expects(newBranch, *, *)
      .returning((List(blockData2), Some(execError)))

    val withMockedBlockExecution = blockImportWithMockedBlockExecution(mockExecution)
    whenReady(withMockedBlockExecution.evaluateBranchBlock(newBlock3).runToFuture)(
      _ shouldEqual BlockEnqueued
    )
    whenReady(withMockedBlockExecution.evaluateBranchBlock(newBlock2).runToFuture) {
      _ shouldBe a[BlockImportFailed]
    }

    blockchainReader.getBestBlock().get shouldEqual oldBlock3
    blockchainReader.getChainWeightByHash(oldBlock3.header.hash) shouldEqual Some(oldWeight3)

    blockQueue.isQueued(newBlock2.header.hash) shouldBe true
    blockQueue.isQueued(newBlock3.header.hash) shouldBe false
  }

  it should "report an orphaned block" in new ImportBlockTestSetup {
    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }

    val newBlock: Block = getBlock(number = bestNum + 1)
    setBlockExists(newBlock, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setChainWeightForBlock(bestBlock, currentWeight)

    (validators.blockHeaderValidator
      .validate(_: BlockHeader, _: GetBlockHeaderByHash)(_: BlockchainConfig))
      .expects(newBlock.header, *, *)
      .returning(Left(HeaderParentNotFoundError))

    whenReady(consensusAdapter.evaluateBranchBlock(newBlock).runToFuture)(
      _ shouldEqual BlockImportFailed("HeaderParentNotFoundError")
    )
  }

  it should "validate blocks prior to import" in new ImportBlockTestSetup {
    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }

    val newBlock: Block = getBlock(number = bestNum + 1)
    setBlockExists(newBlock, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setChainWeightForBlock(bestBlock, currentWeight)

    (validators.blockHeaderValidator
      .validate(_: BlockHeader, _: GetBlockHeaderByHash)(_: BlockchainConfig))
      .expects(newBlock.header, *, *)
      .returning(Left(HeaderDifficultyError))

    whenReady(consensusAdapter.evaluateBranchBlock(newBlock).runToFuture) {
      _ shouldEqual BlockImportFailed(HeaderDifficultyError.toString)
    }
  }

  it should "correctly handle importing genesis block" in new ImportBlockTestSetup {
    val genesisBlock = Block(genesisHeader, BlockBody.empty)

    setBestBlock(genesisBlock)
    setBlockExists(genesisBlock, inChain = true, inQueue = true)

    whenReady(failConsensus.evaluateBranchBlock(genesisBlock).runToFuture)(_ shouldEqual DuplicateBlock)
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

    val weight1 = ChainWeight.totalDifficultyOnly(block1.header.difficulty + 999)
    val oldWeight2 = weight1.increase(oldBlock2.header)
    val oldWeight3 = oldWeight2.increase(oldBlock3.header)

    val newWeight2 = weight1.increase(newBlock2.header)
    val newWeight3 = newWeight2.increase(newBlock3WithOmmer.header)

    blockchainWriter.save(ancestorForValidation, Nil, ChainWeight.totalDifficultyOnly(1), saveAsBestBlock = false)
    blockchainWriter.save(ancestorForValidation1, Nil, ChainWeight.totalDifficultyOnly(3), saveAsBestBlock = false)
    blockchainWriter.save(ancestorForValidation2, Nil, ChainWeight.totalDifficultyOnly(6), saveAsBestBlock = false)

    blockchainWriter.save(block1, Nil, weight1, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock2, receipts, oldWeight2, saveAsBestBlock = true)
    blockchainWriter.save(oldBlock3, Nil, oldWeight3, saveAsBestBlock = true)

    val oldBranch = List(oldBlock2, oldBlock3)
    val newBranch = List(newBlock2, newBlock3WithOmmer)
    val blockData2 = BlockData(newBlock2, Seq.empty[Receipt], newWeight2)
    val blockData3 = BlockData(newBlock3WithOmmer, Seq.empty[Receipt], newWeight3)

    val mockExecution = mock[BlockExecution]
    (mockExecution
      .executeAndValidateBlocks(_: List[Block], _: ChainWeight)(_: BlockchainConfig))
      .expects(newBranch, *, *)
      .returning((List(blockData2, blockData3), None))

    val withMockedBlockExecution = blockImportWithMockedBlockExecution(mockExecution)
    whenReady(withMockedBlockExecution.evaluateBranchBlock(newBlock2).runToFuture)(
      _ shouldEqual BlockEnqueued
    )
    whenReady(withMockedBlockExecution.evaluateBranchBlock(newBlock3WithOmmer).runToFuture) { result =>
      result shouldEqual ChainReorganised(oldBranch, newBranch, List(newWeight2, newWeight3))
    }

    // Saving new blocks, because it's part of executeBlocks method mechanism
    blockchainWriter.save(blockData2.block, blockData2.receipts, blockData2.weight, saveAsBestBlock = true)
    blockchainWriter.save(blockData3.block, blockData3.receipts, blockData3.weight, saveAsBestBlock = true)

    blockchainReader.getBestBlock().get shouldEqual newBlock3WithOmmer
  }

  it should "correctly import a checkpoint block" in new EphemBlockchain with CheckpointHelpers {
    val parentBlock: Block = getBlock(bestNum)
    val regularBlock: Block = getBlock(bestNum + 1, difficulty = 200, parent = parentBlock.hash)
    val checkpointBlock: Block = getCheckpointBlock(parentBlock)

    val weightParent = ChainWeight.totalDifficultyOnly(parentBlock.header.difficulty + 999)
    val weightRegular = weightParent.increase(regularBlock.header)
    val weightCheckpoint = weightParent.increase(checkpointBlock.header)

    blockchainWriter.save(parentBlock, Nil, weightParent, saveAsBestBlock = true)
    blockchainWriter.save(regularBlock, Nil, weightRegular, saveAsBestBlock = true)

    val mockExecution = mock[BlockExecution]
    (mockExecution
      .executeAndValidateBlocks(_: List[Block], _: ChainWeight)(_: BlockchainConfig))
      .expects(List(checkpointBlock), *, *)
      .returning((List(BlockData(checkpointBlock, Nil, weightCheckpoint)), None))

    val withMockedBlockExecution = blockImportWithMockedBlockExecution(mockExecution)
    whenReady(withMockedBlockExecution.evaluateBranchBlock(checkpointBlock).runToFuture) { result =>
      result shouldEqual ChainReorganised(
        List(regularBlock),
        List(checkpointBlock),
        List(weightCheckpoint)
      )
    }

    // Saving new blocks, because it's part of executeBlocks method mechanism
    blockchainWriter.save(checkpointBlock, Nil, weightCheckpoint, saveAsBestBlock = true)

    blockchainReader.getBestBlock().get shouldEqual checkpointBlock
    blockchainReader.getChainWeightByHash(checkpointBlock.hash) shouldEqual Some(weightCheckpoint)
  }

  it should "not import a block with higher difficulty that does not follow a checkpoint" in new EphemBlockchain
    with CheckpointHelpers {

    val parentBlock: Block = getBlock(bestNum)
    val regularBlock: Block = getBlock(bestNum + 1, difficulty = 200, parent = parentBlock.hash)
    val checkpointBlock: Block = getCheckpointBlock(parentBlock)

    val weightParent = ChainWeight.totalDifficultyOnly(parentBlock.header.difficulty + 999)
    val weightCheckpoint = weightParent.increase(checkpointBlock.header)

    blockchainWriter.save(parentBlock, Nil, weightParent, saveAsBestBlock = true)
    blockchainWriter.save(checkpointBlock, Nil, weightCheckpoint, saveAsBestBlock = true)

    val withMockedBlockExecution = blockImportWithMockedBlockExecution(mock[BlockExecution])
    whenReady(withMockedBlockExecution.evaluateBranchBlock(regularBlock).runToFuture)(
      _ shouldEqual BlockEnqueued
    )

    blockchainReader.getBestBlock().get shouldEqual checkpointBlock
  }

  trait ImportBlockTestSetup extends TestSetupWithVmAndValidators with MockBlockchain

}
