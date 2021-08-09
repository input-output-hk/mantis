package io.iohk.ethereum.ledger

import akka.util.ByteString

import org.scalamock.handlers.CallHandler0
import org.scalamock.handlers.CallHandler1
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.SyncConfig

class BlockQueueSpec extends AnyFlatSpec with Matchers with MockFactory {

  "BlockQueue" should "ignore block if it's already in the queue" in new TestConfig {
    val block = getBlock(1)
    val parentWeight = ChainWeight.zero
    setBestBlockNumber(1).twice()
    setChainWeightForParent(block, Some(parentWeight))

    blockQueue.enqueueBlock(block) shouldEqual Some(Leaf(block.header.hash, parentWeight.increase(block.header)))
    blockQueue.enqueueBlock(block) shouldEqual None
    blockQueue.isQueued(block.header.hash) shouldBe true
  }

  it should "ignore blocks outside of range" in new TestConfig {
    val block1 = getBlock(1)
    val block30 = getBlock(30)
    setBestBlockNumber(15).twice()

    blockQueue.enqueueBlock(block1)
    blockQueue.isQueued(block1.header.hash) shouldBe false

    blockQueue.enqueueBlock(block30)
    blockQueue.isQueued(block30.header.hash) shouldBe false
  }

  it should "remove the blocks that fall out of range" in new TestConfig {
    val block1 = getBlock(1)
    setBestBlockNumber(1)
    setChainWeightForParent(block1)

    blockQueue.enqueueBlock(block1)
    blockQueue.isQueued(block1.header.hash) shouldBe true

    val block20 = getBlock(20)
    setBestBlockNumber(20)
    setChainWeightForParent(block20)

    blockQueue.enqueueBlock(block20)
    blockQueue.isQueued(block20.header.hash) shouldBe true
    blockQueue.isQueued(block1.header.hash) shouldBe false
  }

  it should "enqueue a block with parent on the main chain updating its total difficulty" in new TestConfig {
    val block1 = getBlock(1, 13)
    val parentWeight = ChainWeight.totalDifficultyOnly(42)
    setBestBlockNumber(1)
    setChainWeightForParent(block1, Some(parentWeight))

    blockQueue.enqueueBlock(block1) shouldEqual Some(Leaf(block1.header.hash, parentWeight.increase(block1.header)))
  }

  it should "enqueue a block with queued ancestors rooted to the main chain updating its total difficulty" in new TestConfig {
    val block1 = getBlock(1, 101)
    val block2a = getBlock(2, 102, block1.header.hash)
    val block2b = getBlock(2, 99, block1.header.hash)
    val block3 = getBlock(3, 103, block2a.header.hash)

    val parentWeight = ChainWeight.totalDifficultyOnly(42)

    setBestBlockNumber(1).anyNumberOfTimes()
    setChainWeightForParent(block1, Some(parentWeight))
    setChainWeightForParent(block2a, None)
    setChainWeightForParent(block2b, None)
    setChainWeightForParent(block3, None)

    blockQueue.enqueueBlock(block1)
    blockQueue.enqueueBlock(block2a)
    blockQueue.enqueueBlock(block2b)

    val expectedWeight = List(block1, block2a, block3).map(_.header).foldLeft(parentWeight)(_ increase _)
    blockQueue.enqueueBlock(block3) shouldEqual Some(Leaf(block3.header.hash, expectedWeight))
  }

  it should "enqueue an orphaned block" in new TestConfig {
    val block1 = getBlock(1)
    setBestBlockNumber(1)
    setChainWeightForParent(block1)

    blockQueue.enqueueBlock(block1) shouldBe None
    blockQueue.isQueued(block1.header.hash) shouldBe true
  }

  it should "remove a branch from a leaf up to the first shared ancestor" in new TestConfig {
    val block1 = getBlock(1)
    val block2a = getBlock(2, parent = block1.header.hash)
    val block2b = getBlock(2, parent = block1.header.hash)
    val block3 = getBlock(3, parent = block2a.header.hash)

    setBestBlockNumber(1).anyNumberOfTimes()
    setChainWeightForParent(block1)
    setChainWeightForParent(block2a)
    setChainWeightForParent(block2b)
    setChainWeightForParent(block3)

    blockQueue.enqueueBlock(block1)
    blockQueue.enqueueBlock(block2a)
    blockQueue.enqueueBlock(block2b)
    blockQueue.enqueueBlock(block3)

    blockQueue.getBranch(block3.header.hash, dequeue = true) shouldEqual List(block1, block2a, block3)

    blockQueue.isQueued(block3.header.hash) shouldBe false
    blockQueue.isQueued(block2a.header.hash) shouldBe false
    blockQueue.isQueued(block2b.header.hash) shouldBe true
    blockQueue.isQueued(block1.header.hash) shouldBe true
  }

  it should "remove a whole subtree down from an ancestor to all its leaves" in new TestConfig {
    val block1a = getBlock(1)
    val block1b = getBlock(1)
    val block2a = getBlock(2, parent = block1a.header.hash)
    val block2b = getBlock(2, parent = block1a.header.hash)
    val block3 = getBlock(3, parent = block2a.header.hash)

    setBestBlockNumber(1).anyNumberOfTimes()
    setChainWeightForParent(block1a)
    setChainWeightForParent(block1b)
    setChainWeightForParent(block2a)
    setChainWeightForParent(block2b)
    setChainWeightForParent(block3)

    blockQueue.enqueueBlock(block1a)
    blockQueue.enqueueBlock(block1b)
    blockQueue.enqueueBlock(block2a)
    blockQueue.enqueueBlock(block2b)
    blockQueue.enqueueBlock(block3)

    blockQueue.isQueued(block3.header.hash) shouldBe true
    blockQueue.isQueued(block2a.header.hash) shouldBe true
    blockQueue.isQueued(block2b.header.hash) shouldBe true
    blockQueue.isQueued(block1a.header.hash) shouldBe true
    blockQueue.isQueued(block1b.header.hash) shouldBe true

    blockQueue.removeSubtree(block1a.header.hash)

    blockQueue.isQueued(block3.header.hash) shouldBe false
    blockQueue.isQueued(block2a.header.hash) shouldBe false
    blockQueue.isQueued(block2b.header.hash) shouldBe false
    blockQueue.isQueued(block1a.header.hash) shouldBe false
    blockQueue.isQueued(block1b.header.hash) shouldBe true
  }

  trait TestConfig {
    val syncConfig: SyncConfig =
      SyncConfig(Config.config).copy(maxQueuedBlockNumberAhead = 10, maxQueuedBlockNumberBehind = 10)
    val blockchainReader: BlockchainReader = mock[BlockchainReader]
    val blockchain: BlockchainImpl = mock[BlockchainImpl]
    val blockQueue: BlockQueue = BlockQueue(blockchain, blockchainReader, syncConfig)

    def setBestBlockNumber(n: BigInt): CallHandler0[BigInt] =
      (blockchainReader.getBestBlockNumber _).expects().returning(n)

    def setChainWeightForParent(
        block: Block,
        weight: Option[ChainWeight] = None
    ): CallHandler1[ByteString, Option[ChainWeight]] =
      (blockchainReader.getChainWeightByHash _).expects(block.header.parentHash).returning(weight)

    def randomHash(): ByteString =
      ObjectGenerators.byteStringOfLengthNGen(32).sample.get

    val defaultHeader: BlockHeader = Fixtures.Blocks.ValidBlock.header.copy(
      difficulty = 1000000,
      number = 1,
      gasLimit = 1000000,
      gasUsed = 0,
      unixTimestamp = 0
    )

    def getBlock(
        number: BigInt,
        difficulty: BigInt = 1000000,
        parent: ByteString = randomHash(),
        salt: ByteString = randomHash()
    ): Block =
      Block(
        defaultHeader.copy(parentHash = parent, difficulty = difficulty, number = number, extraData = salt),
        BlockBody.empty
      )
  }

}
