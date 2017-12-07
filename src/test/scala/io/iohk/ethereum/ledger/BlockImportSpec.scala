package io.iohk.ethereum.ledger

import akka.util.ByteString
import akka.util.ByteString.{empty => bEmpty}
import io.iohk.ethereum.blockchain.sync.EphemBlockchainTestSetup
import io.iohk.ethereum.{Mocks, ObjectGenerators}
import io.iohk.ethereum.domain.{Block, BlockHeader, BlockchainImpl, Receipt}
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationAfterExecError
import io.iohk.ethereum.ledger.BlockQueue.Leaf
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.utils.{BlockchainConfig, Config}
import io.iohk.ethereum.validators.BlockHeaderError.{HeaderDifficultyError, HeaderParentNotFoundError}
import io.iohk.ethereum.validators.BlockHeaderValidator
//import io.iohk.ethereum.validators.BlockHeaderError.HeaderDifficultyError
//import io.iohk.ethereum.validators._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.language.reflectiveCalls
import scala.collection.mutable


class BlockImportSpec extends FlatSpec with Matchers with MockFactory {

  "Importing blocks" should "ignore existing block" in new TestSetup with MockBlockchain {
    val block1 = getBlock()
    val block2 = getBlock()

    setBlockExists(block1, inChain = true, inQueue = false)
    ledger.importBlock(block1) shouldEqual DuplicateBlock

    setBlockExists(block2, inChain = false, inQueue = true)
    ledger.importBlock(block2) shouldEqual DuplicateBlock
  }

  it should "import a block to top of the main chain" in new TestSetup with MockBlockchain {
    val block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, false, false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)
    ledger.setExecutionResult(block, Right(receipts))

    (blockQueue.enqueueBlock _).expects(block, bestNum)
      .returning(Some(Leaf(block.header.hash, currentTd + block.header.difficulty)))
    (blockQueue.getBranch _).expects(block.header.hash, true).returning(List(block))

    val newTd = currentTd + block.header.difficulty
    expectBlockSaved(block, receipts, newTd, true)

    ledger.importBlock(block) shouldEqual BlockImportedToTop(List(block), List(newTd))
  }

  it should "handle exec error when importing to top" in new TestSetup with MockBlockchain {
    val block = getBlock(6, parent = bestBlock.header.hash)

    setBlockExists(block, false, false)
    setBestBlock(bestBlock)
    setTotalDifficultyForBlock(bestBlock, currentTd)
    ledger.setExecutionResult(block, Left(execError))

    (blockQueue.enqueueBlock _).expects(block, bestNum)
      .returning(Some(Leaf(block.header.hash, currentTd + block.header.difficulty)))
    (blockQueue.getBranch _).expects(block.header.hash, true).returning(List(block))
    (blockQueue.removeSubtree _).expects(block.header.hash)

    ledger.importBlock(block) shouldEqual BlockImportFailed(execError.toString)
  }

  it should "reorganise chain when a newly enqueued block forms a better branch" in new TestSetup with EphemBlockchain {
    val block1 = getBlock(bestNum - 2, difficulty = 100)
    val newBlock2 = getBlock(bestNum - 1, difficulty = 101, parent = block1.header.hash)
    val newBlock3 = getBlock(bestNum, difficulty = 105, parent = newBlock2.header.hash)
    val oldBlock2 = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)
    val oldBlock3 = getBlock(bestNum, difficulty = 103, parent = oldBlock2.header.hash)

    val td1 = block1.header.difficulty + 999
    val newTd2 = td1 + newBlock2.header.difficulty
    val newTd3 = newTd2 + newBlock3.header.difficulty
    val oldTd2 = td1 + oldBlock2.header.difficulty
    val oldTd3 = oldTd2 + oldBlock3.header.difficulty

    blockchain.save(block1, Nil, td1, true)
    blockchain.save(oldBlock2, receipts, oldTd2, true)
    blockchain.save(oldBlock3, Nil, oldTd3, true)

    val ancestorForValidation = getBlock(0, difficulty = 1)
    blockchain.save(ancestorForValidation, Nil, 1, false)

    ledger.setExecutionResult(newBlock2, Right(Nil))
    ledger.setExecutionResult(newBlock3, Right(receipts))

    ledger.importBlock(newBlock3) shouldEqual BlockEnqueued
    ledger.importBlock(newBlock2) shouldEqual
      ChainReorganised(List(oldBlock2, oldBlock3), List(newBlock2, newBlock3), List(newTd2, newTd3))

    blockchain.getBestBlock() shouldEqual newBlock3
    blockchain.getTotalDifficultyByHash(newBlock3.header.hash) shouldEqual Some(newTd3)

    blockQueue.isQueued(oldBlock2.header.hash) shouldBe true
    blockQueue.isQueued(oldBlock3.header.hash) shouldBe true
  }

  it should "handle error when trying to reorganise chain" in new TestSetup with EphemBlockchain {
    val block1 = getBlock(bestNum - 2, difficulty = 100)
    val newBlock2 = getBlock(bestNum - 1, difficulty = 101, parent = block1.header.hash)
    val newBlock3 = getBlock(bestNum, difficulty = 105, parent = newBlock2.header.hash)
    val oldBlock2 = getBlock(bestNum - 1, difficulty = 102, parent = block1.header.hash)
    val oldBlock3 = getBlock(bestNum, difficulty = 103, parent = oldBlock2.header.hash)

    val td1 = block1.header.difficulty + 999
    val newTd2 = td1 + newBlock2.header.difficulty
    val newTd3 = newTd2 + newBlock3.header.difficulty
    val oldTd2 = td1 + oldBlock2.header.difficulty
    val oldTd3 = oldTd2 + oldBlock3.header.difficulty

    blockchain.save(block1, Nil, td1, true)
    blockchain.save(oldBlock2, receipts, oldTd2, true)
    blockchain.save(oldBlock3, Nil, oldTd3, true)

    val ancestorForValidation = getBlock(0, difficulty = 1)
    blockchain.save(ancestorForValidation, Nil, 1, false)

    ledger.setExecutionResult(newBlock2, Right(Nil))
    ledger.setExecutionResult(newBlock3, Left(execError))

    ledger.importBlock(newBlock3) shouldEqual BlockEnqueued
    ledger.importBlock(newBlock2) shouldBe a[BlockImportFailed]

    blockchain.getBestBlock() shouldEqual oldBlock3
    blockchain.getTotalDifficultyByHash(oldBlock3.header.hash) shouldEqual Some(oldTd3)

    blockQueue.isQueued(newBlock2.header.hash) shouldBe true
    blockQueue.isQueued(newBlock3.header.hash) shouldBe false
  }

  it should "report an orphaned block" in new TestSetup with MockBlockchain {
    val validators = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }
    val ledgerWithMockedValidators = new LedgerImpl(new Mocks.MockVM(), blockchain, blockQueue, blockchainConfig, validators)

    val newBlock = getBlock()

    (validators.blockHeaderValidator.validate(_: BlockHeader, _: ByteString => Option[BlockHeader]))
      .expects(newBlock.header, *).returning(Left(HeaderParentNotFoundError))

    ledgerWithMockedValidators.importBlock(newBlock) shouldEqual UnknownParent
  }

  it should "validate blocks prior to import" in new TestSetup with MockBlockchain {
    val validators = new Mocks.MockValidatorsAlwaysSucceed {
      override val blockHeaderValidator: BlockHeaderValidator = mock[BlockHeaderValidator]
    }
    val ledgerWithMockedValidators = new LedgerImpl(new Mocks.MockVM(), blockchain, blockQueue, blockchainConfig, validators)

    val newBlock = getBlock()

    (validators.blockHeaderValidator.validate(_: BlockHeader, _: ByteString => Option[BlockHeader]))
      .expects(newBlock.header, *).returning(Left(HeaderDifficultyError))

    ledgerWithMockedValidators.importBlock(newBlock) shouldEqual BlockImportFailed(HeaderDifficultyError.toString)
  }


  "Branch resolution" should "report an invalid branch when headers do not form a chain" in new TestSetup with MockBlockchain {
    val headers = getChainHeaders(1, 10).reverse
    ledger.resolveBranch(headers) shouldEqual InvalidBranch
  }

  it should "report an invalid branch when headers do not reach the current best block number" in new TestSetup with MockBlockchain {
    val headers = getChainHeaders(1, 10)
    setBestBlockNumber(11)

    ledger.resolveBranch(headers) shouldEqual InvalidBranch
  }

  it should "report an unknown branch in the parent of the first header is unknown" in new TestSetup with MockBlockchain {
    val headers = getChainHeaders(5, 10)
    setBestBlockNumber(10)
    setHeaderByHash(headers.head.parentHash, None)

    ledger.resolveBranch(headers) shouldEqual UnknownBranch
  }

  it should "report new better branch found when headers form a branch of higher difficulty than corresponding know headers" in
    new TestSetup with MockBlockchain {
      val headers = getChainHeaders(1, 10)
      setBestBlockNumber(10)
      setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

      val oldBlocks = headers.map(h => getBlock(h.number, h.difficulty - 1))
      oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

      ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
    }

  it should "report no need for a chain switch the headers do not have difficulty greater than currently known branch" in
    new TestSetup with MockBlockchain {
      val headers = getChainHeaders(1, 10)
      setBestBlockNumber(10)
      setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

      val oldBlocks = headers.map(h => getBlock(h.number, h.difficulty))
      oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

      ledger.resolveBranch(headers) shouldEqual NoChainSwitch
    }

  it should "correctly handle a branch that goes up to the genesis block" in
    new TestSetup with MockBlockchain {
      val headers = genesisHeader :: getChainHeaders(1, 10, genesisHeader.hash)

      setGenesisHeader(genesisHeader)
      setBestBlockNumber(10)
      val oldBlocks = headers.tail.map(h => getBlock(h.number, h.difficulty - 1))
      oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))
      setBlockByNumber(0, Some(Block(genesisHeader, BlockBody(Nil, Nil))))

      ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
    }

  it should "report an unknown branch if the included genesis header is different than ours" in
    new TestSetup with MockBlockchain {
      val differentGenesis = genesisHeader.copy(extraData = ByteString("I'm different ;("))
      val headers = differentGenesis :: getChainHeaders(1, 10, differentGenesis.hash)

      setGenesisHeader(genesisHeader)
      setBestBlockNumber(10)

      ledger.resolveBranch(headers) shouldEqual UnknownBranch
    }

  it should "not include common prefix as result when finding a new better branch" in
    new TestSetup with MockBlockchain {
      val headers = getChainHeaders(1, 10)
      setBestBlockNumber(8)
      setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

      val oldBlocks = headers.slice(2, 8).map(h => getBlock(h.number, h.difficulty - 1))
      oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))
      setBlockByNumber(1, Some(Block(headers(0), BlockBody(Nil, Nil))))
      setBlockByNumber(2, Some(Block(headers(1), BlockBody(Nil, Nil))))
      setBlockByNumber(9, None)

      ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
      assert(oldBlocks.map(_.header.number) == List[BigInt](3, 4, 5, 6, 7, 8))
    }


  trait TestSetup {
    val blockchainConfig = BlockchainConfig(Config.config)
    val blockQueue: BlockQueue
    val blockchain: BlockchainImpl

    lazy val ledger = new LedgerImpl(new Mocks.MockVM(), blockchain, blockQueue, blockchainConfig, Mocks.MockValidatorsAlwaysSucceed) {
      private val results = mutable.Map[ByteString, Either[BlockExecutionError, Seq[Receipt]]]()

      override def executeBlock(block: Block, alreadyValidated: Boolean = false): Either[BlockExecutionError, Seq[Receipt]] =
        results(block.header.hash)

      def setExecutionResult(block: Block, result: Either[BlockExecutionError, Seq[Receipt]]): Unit =
        results(block.header.hash) = result
    }

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
      difficulty = 100,
      number = 1,
      gasLimit = 1000000,
      gasUsed = 0,
      unixTimestamp = 0,
      extraData = bEmpty,
      mixHash = bEmpty,
      nonce = bEmpty
    )

    val genesisHeader = defaultHeader.copy(number = 0, extraData = ByteString("genesis"))

    def getBlock(
      number: BigInt = 1,
      difficulty: BigInt = 100,
      parent: ByteString = randomHash(),
      salt: ByteString = randomHash()): Block =
      Block(
        defaultHeader.copy(
          parentHash = parent,
          difficulty = difficulty,
          number = number,
          extraData = salt),
        BlockBody(Nil, Nil))

    def getChain(from: BigInt, to: BigInt, parent: ByteString = randomHash()): List[Block] =
      if (from > to)
        Nil
      else {
        val block = getBlock(from, parent = parent)
        block :: getChain(from + 1, to, block.header.hash)
      }

    def getChainHeaders(from: BigInt, to: BigInt, parent: ByteString = randomHash()): List[BlockHeader] =
      getChain(from, to, parent).map(_.header)

    val receipts = Seq(Receipt(randomHash(), 50000, randomHash(), Nil))

    val currentTd = 99999

    val bestNum = BigInt(5)

    val bestBlock = getBlock(bestNum, currentTd / 2)

    val execError = ValidationAfterExecError("error")
  }

  trait EphemBlockchain extends EphemBlockchainTestSetup { self: TestSetup =>
    val blockQueue = BlockQueue(blockchain, SyncConfig(Config.config))
  }

  trait MockBlockchain { self: TestSetup =>
    val blockchain = mock[BlockchainImpl]
    class MockBlockQueue extends BlockQueue(null, 10, 10)
    val blockQueue: BlockQueue = mock[MockBlockQueue]

    def setBlockExists(block: Block, inChain: Boolean, inQueue: Boolean) = {
      (blockchain.getBlockByHash _).expects(block.header.hash).anyNumberOfTimes().returning(Some(block).filter(_ => inChain))
      (blockQueue.isQueued _).expects(block.header.hash).anyNumberOfTimes().returning(inQueue)
    }

    def setBestBlock(block: Block) = {
      (blockchain.getBestBlock _).expects().returning(block)
      (blockchain.getBestBlockNumber _).expects().anyNumberOfTimes().returning(block.header.number)
    }

    def setBestBlockNumber(num: BigInt) =
      (blockchain.getBestBlockNumber _).expects().returning(num)

    def setTotalDifficultyForBlock(block: Block, td: BigInt) =
      (blockchain.getTotalDifficultyByHash _).expects(block.header.hash).returning(Some(td))

    def expectBlockSaved(block: Block, receipts: Seq[Receipt], td: BigInt, saveAsBestBlock: Boolean) = {
      (blockchain.save(_: Block, _: Seq[Receipt], _: BigInt, _: Boolean))
        .expects(block, receipts, td, saveAsBestBlock).once()
    }

    def setHeaderByHash(hash: ByteString, header: Option[BlockHeader]) =
      (blockchain.getBlockHeaderByHash _).expects(hash).returning(header)

    def setBlockByNumber(number: BigInt, block: Option[Block]) =
      (blockchain.getBlockByNumber _).expects(number).returning(block)

    def setGenesisHeader(header: BlockHeader) = {
      (blockchain.getBlockHeaderByNumber _).expects(BigInt(0)).returning(Some(header))
      setHeaderByHash(header.parentHash, None)
    }
  }
}
