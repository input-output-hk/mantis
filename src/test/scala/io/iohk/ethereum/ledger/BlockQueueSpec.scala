package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockchainImpl}
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.Config.SyncConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class BlockQueueSpec extends FlatSpec with Matchers with MockFactory {

  "BlockQueue" should "ignore block if it's already in the queue" in new TestConfig {
    val block = getBlock(1)
    setBestBlockNumber(1).twice()
    setTotalDifficultyForParent(block, Some(0))

    blockQueue.enqueueBlock(block) shouldEqual Some(Leaf(block.header.hash, block.header.difficulty))
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
    setTotalDifficultyForParent(block1)

    blockQueue.enqueueBlock(block1)
    blockQueue.isQueued(block1.header.hash) shouldBe true

    val block20 = getBlock(20)
    setBestBlockNumber(20)
    setTotalDifficultyForParent(block20)

    blockQueue.enqueueBlock(block20)
    blockQueue.isQueued(block20.header.hash) shouldBe true
    blockQueue.isQueued(block1.header.hash) shouldBe false
  }

  it should "enqueue a block with parent on the main chain updating its total difficulty" in new TestConfig {
    val block1 = getBlock(1, 13)
    setBestBlockNumber(1)
    setTotalDifficultyForParent(block1, Some(42))

    blockQueue.enqueueBlock(block1) shouldEqual Some(Leaf(block1.header.hash, block1.header.difficulty + 42))
  }


  it should "enqueue a block with queued ancestors rooted to the main chain updating its total difficulty" in new TestConfig {
    val block1 = getBlock(1, 101)
    val block2a = getBlock(2, 102, block1.header.hash)
    val block2b = getBlock(2, 99, block1.header.hash)
    val block3 = getBlock(3, 103, block2a.header.hash)

    setBestBlockNumber(1).anyNumberOfTimes()
    setTotalDifficultyForParent(block1, Some(42))
    setTotalDifficultyForParent(block2a, None)
    setTotalDifficultyForParent(block2b, None)
    setTotalDifficultyForParent(block3, None)

    blockQueue.enqueueBlock(block1)
    blockQueue.enqueueBlock(block2a)
    blockQueue.enqueueBlock(block2b)

    val expectedTd = 42 + List(block1, block2a, block3).map(_.header.difficulty).sum
    blockQueue.enqueueBlock(block3) shouldEqual Some(Leaf(block3.header.hash, expectedTd))
  }

  it should "enqueue an orphaned block" in new TestConfig {
    val block1 = getBlock(1)
    setBestBlockNumber(1)
    setTotalDifficultyForParent(block1)

    blockQueue.enqueueBlock(block1) shouldBe None
    blockQueue.isQueued(block1.header.hash) shouldBe true
  }

  it should "remove a branch from a leaf up to the first shared ancestor" in new TestConfig {
    val block1 = getBlock(1)
    val block2a = getBlock(2, parent = block1.header.hash)
    val block2b = getBlock(2, parent = block1.header.hash)
    val block3 = getBlock(3, parent = block2a.header.hash)

    setBestBlockNumber(1).anyNumberOfTimes()
    setTotalDifficultyForParent(block1)
    setTotalDifficultyForParent(block2a)
    setTotalDifficultyForParent(block2b)
    setTotalDifficultyForParent(block3)

    blockQueue.enqueueBlock(block1)
    blockQueue.enqueueBlock(block2a)
    blockQueue.enqueueBlock(block2b)
    blockQueue.enqueueBlock(block3)

    blockQueue.removeBranch(block3.header.hash) shouldEqual List(block1, block2a, block3)

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
    setTotalDifficultyForParent(block1a)
    setTotalDifficultyForParent(block1b)
    setTotalDifficultyForParent(block2a)
    setTotalDifficultyForParent(block2b)
    setTotalDifficultyForParent(block3)

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
    val syncConfig = SyncConfig(Config.config)
    val blockchain = mock[BlockchainImpl]
    val blockQueue = BlockQueue(blockchain, syncConfig)

    def setBestBlockNumber(n: BigInt) =
      (blockchain.getBestBlockNumber _).expects().returning(n)

    def setTotalDifficultyForParent(block: Block, td: Option[BigInt] = None) =
      (blockchain.getTotalDifficultyByHash _).expects(block.header.parentHash).returning(td)

    def randomHash(): ByteString =
      ObjectGenerators.byteStringOfLengthNGen(32).sample.get

    val defaultHeader = BlockHeader(
      parentHash = bEmpty,
      ommersHash = bEmpty,
      beneficiary = bEmpty,
      stateRoot = bEmpty,
      transactionsRoot = bEmpty,
      receiptsRoot = bEmpty,
      logsBloom = bEmpty,
      difficulty = 1000000,
      number = 1,
      gasLimit = 1000000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = bEmpty,
      mixHash = bEmpty,
      nonce = bEmpty
    )

    def getBlock(
        number: BigInt,
        difficulty: BigInt = 1000000,
        parent: ByteString = randomHash(),
        salt: ByteString = randomHash()): Block =
      Block(
        defaultHeader.copy(
          parentHash = parent,
          difficulty = difficulty,
          number = number,
          extraData = salt),
        BlockBody(Nil, Nil))
  }

}
