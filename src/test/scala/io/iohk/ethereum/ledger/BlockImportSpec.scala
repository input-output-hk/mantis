package io.iohk.ethereum.ledger

import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.consensus._
import io.iohk.ethereum.consensus.validators.BlockHeaderError.{ HeaderDifficultyError, HeaderParentNotFoundError }
import io.iohk.ethereum.consensus.validators._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import org.scalamock.scalatest.MockFactory
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ FlatSpec, Matchers }

class BlockImportSpec extends FlatSpec with Matchers with MockFactory with ScalaFutures {

  "Importing blocks" should "ignore existing block" in new TestSetupWithVmAndValidators with MockBlockchain {
    val block1: Block = getBlock()
    val block2: Block = getBlock()

    setBlockExists(block1, inChain = true, inQueue = false)
    setBestBlock(bestBlock)

    whenReady(ledger.importBlock(block1)){result => result shouldEqual DuplicateBlock}

    setBlockExists(block2, inChain = false, inQueue = true)
    setBestBlock(bestBlock)
    whenReady(ledger.importBlock(block2)){result => result shouldEqual DuplicateBlock}
  }

  it should "import a block to top of the main chain" in new TestSetupWithVmAndValidators with MockBlockchain {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)
    ledger.setExecutionResult(block, Right(receipts))

    (blockQueue.enqueueBlock _).expects(block, bestNum)
      .returning(Some(Leaf(block.header.hash, currentTd + block.header.difficulty)))
    (blockQueue.getBranch _).expects(block.header.hash, true).returning(List(block))

    val newTd: BigInt = currentTd + block.header.difficulty
    val blockData = BlockData(block, receipts, newTd)
    expectBlockSaved(block, receipts, newTd, saveAsBestBlock = true)

    whenReady(ledger.importBlock(block)){result => result shouldEqual BlockImportedToTop(List(blockData)) }
  }

  it should "handle exec error when importing to top" in new TestSetupWithVmAndValidators with MockBlockchain {
    val block: Block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, inChain = false, inQueue = false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)
    ledger.setExecutionResult(block, Left(execError))

    (blockQueue.enqueueBlock _).expects(block, bestNum)
      .returning(Some(Leaf(block.header.hash, currentTd + block.header.difficulty)))
    (blockQueue.getBranch _).expects(block.header.hash, true).returning(List(block))
    (blockQueue.removeSubtree _).expects(block.header.hash)

    whenReady(ledger.importBlock(block)){result => result shouldEqual BlockImportFailed(execError.toString)}
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

    ledger.setExecutionResult(newBlock2, Right(Nil))
    ledger.setExecutionResult(newBlock3, Right(receipts))

    whenReady(ledger.importBlock(newBlock3)){result => result shouldEqual BlockEnqueued}
    whenReady(ledger.importBlock(newBlock2)){result => result shouldEqual
      ChainReorganised(List(oldBlock2, oldBlock3), List(newBlock2, newBlock3), List(newTd2, newTd3))}

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

    ledger.setExecutionResult(newBlock2, Right(Nil))
    ledger.setExecutionResult(newBlock3, Left(execError))

    whenReady(ledger.importBlock(newBlock3)){result =>
      result shouldEqual BlockEnqueued
    }

    whenReady(ledger.importBlock(newBlock2)){ result =>
      result shouldBe a[BlockImportFailed]
    }

    blockchain.getBestBlock() shouldEqual oldBlock3
    blockchain.getTotalDifficultyByHash(oldBlock3.header.hash) shouldEqual Some(oldTd3)

    blockQueue.isQueued(newBlock2.header.hash) shouldBe true
    blockQueue.isQueued(newBlock3.header.hash) shouldBe false
  }

  it should "report an orphaned block" in new TestSetupWithVmAndValidators with MockBlockchain {
    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }

    val ledgerWithMockedValidators = new LedgerImpl(
      blockchain,
      blockQueue,
      blockchainConfig,
      consensus.withValidators(validators).withVM(new Mocks.MockVM()),
      testContext
    )

    val newBlock: Block = getBlock(number = bestNum + 1)
    setBlockExists(newBlock, inChain = false, inQueue = false)

    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)
    (validators.blockHeaderValidator.validate(_: BlockHeader, _: GetBlockHeaderByHash))
      .expects(newBlock.header, *).returning(Left(HeaderParentNotFoundError))

    whenReady(ledgerWithMockedValidators.importBlock(newBlock)){ result =>
      result shouldEqual UnknownParent
    }
  }

  it should "validate blocks prior to import" in new TestSetupWithVmAndValidators with MockBlockchain {
    override lazy val validators: MockValidatorsAlwaysSucceed = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }
    val ledgerWithMockedValidators = new LedgerImpl(
      blockchain, blockQueue, blockchainConfig,
      consensus.withValidators(validators).withVM(new Mocks.MockVM()), testContext
    )

    val newBlock: Block = getBlock(number = bestNum + 1)
    setBlockExists(newBlock, inChain = false, inQueue = false)

    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)
    (validators.blockHeaderValidator.validate(_: BlockHeader, _: GetBlockHeaderByHash))
      .expects(newBlock.header, *).returning(Left(HeaderDifficultyError))

    whenReady(ledgerWithMockedValidators.importBlock(newBlock)){result =>
      result shouldEqual BlockImportFailed(HeaderDifficultyError.toString)}
  }
}
