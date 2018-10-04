package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.{ Block, BlockHeader }
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ Matchers, WordSpec }

class BranchResolutionSpec extends WordSpec with Matchers with ObjectGenerators with ScalaFutures {

  "BranchResolution" should { // todo: (AK) 4 failing tests to fix

    "check if headers are from chain" in new BlockchainSetup {
      val parent: BlockHeader = defaultBlockHeader.copy(number = 1)
      val child: BlockHeader = defaultBlockHeader.copy(number = 2, parentHash = parent.hash)
      ledger.branchResolution.areHeadersFormChain(Seq(parent, child)) shouldBe true
    }

    "check if headers are not from chain" in new BlockchainSetup {
      val parent: BlockHeader = defaultBlockHeader.copy(number = 1)
      val otherParent: BlockHeader = defaultBlockHeader.copy(number = 3)
      val child: BlockHeader = defaultBlockHeader.copy(number = 2, parentHash = parent.hash)
      ledger.branchResolution.areHeadersFormChain(Seq(otherParent, child)) shouldBe false
    }

    "check if headers are not empty" in new BlockchainSetup {
      ledger.branchResolution.areHeadersFormChain(Seq.empty) shouldBe false
    }

    "report an invalid branch when headers do not form a chain" in new TestSetupWithVmAndValidators with MockBlockchain {
      val headers: List[BlockHeader] = getChainHeaders(1, 10).reverse
      ledger.resolveBranch(headers) shouldEqual InvalidBranch
    }

    // scalastyle:off magic.number
    "report an invalid branch when headers do not reach the current best block number" in new TestSetupWithVmAndValidators with MockBlockchain {
      val headers: List[BlockHeader] = getChainHeaders(1, 10)
      setBestBlockNumber(11)

      ledger.resolveBranch(headers) shouldEqual InvalidBranch
    }

    "report an unknown branch in the parent of the first header is unknown" in new TestSetupWithVmAndValidators with MockBlockchain {
      val headers: List[BlockHeader] = getChainHeaders(5, 10)

      setGenesisHeader(genesisHeader) // Check genesis block
      setBestBlockNumber(10)
      setHeaderByHash(headers.head.parentHash, None)

      ledger.resolveBranch(headers) shouldEqual UnknownBranch
    }

    "report new better branch found when headers form a branch of higher difficulty than corresponding know headers" in
      new TestSetupWithVmAndValidators with MockBlockchain {
        val headers: List[BlockHeader] = getChainHeaders(1, 10)

        setGenesisHeader(genesisHeader) // Check genesis block

        setBestBlockNumber(10)
        setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

        val oldBlocks: List[Block] = headers.map(h => getBlock(h.number, h.difficulty - 1))
        oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

        ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
      }

    "report no need for a chain switch the headers do not have difficulty greater than currently known branch" in
      new TestSetupWithVmAndValidators with MockBlockchain {
        val headers: List[BlockHeader] = getChainHeaders(1, 10)

        setGenesisHeader(genesisHeader) // Check genesis block

        setBestBlockNumber(10)
        setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

        val oldBlocks: List[Block] = headers.map(h => getBlock(h.number, h.difficulty))
        oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

        ledger.resolveBranch(headers) shouldEqual NoChainSwitch
      }

    "correctly handle a branch that goes up to the genesis block" in
      new TestSetupWithVmAndValidators with MockBlockchain {
        val headers: List[BlockHeader] = genesisHeader :: getChainHeaders(1, 10, genesisHeader.hash)

        setGenesisHeader(genesisHeader)
        setBestBlockNumber(10)
        val oldBlocks: List[Block] = headers.tail.map(h => getBlock(h.number, h.difficulty - 1))
        oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))
        setBlockByNumber(0, Some(Block(genesisHeader, BlockBody(Nil, Nil))))

        ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
      }

    "correctly handle importing genesis block" in
      new TestSetupWithVmAndValidators with MockBlockchain {
        val genesisBlock = Block(genesisHeader, BlockBody(Nil, Nil))

        setBestBlock(genesisBlock)
        setBlockExists(genesisBlock, inChain = true, inQueue = true)

        whenReady(failLedger.importBlock(genesisBlock)){result =>
          result shouldEqual DuplicateBlock
        }
      }

    "report an unknown branch if the included genesis header is different than ours" in
      new TestSetupWithVmAndValidators with MockBlockchain {
        val differentGenesis: BlockHeader = genesisHeader.copy(extraData = ByteString("I'm different ;("))
        val headers: List[BlockHeader] = differentGenesis :: getChainHeaders(1, 10, differentGenesis.hash)

        setGenesisHeader(genesisHeader)
        setBestBlockNumber(10)

        ledger.resolveBranch(headers) shouldEqual UnknownBranch
      }

    "not include common prefix as result when finding a new better branch" in
      new TestSetupWithVmAndValidators with MockBlockchain {
        val headers: List[BlockHeader] = getChainHeaders(1, 10)

        setGenesisHeader(genesisHeader) // Check genesis block

        setBestBlockNumber(8)
        setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

        val oldBlocks: List[Block] = headers.slice(2, 8).map(h => getBlock(h.number, h.difficulty - 1))
        oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))
        setBlockByNumber(1, Some(Block(headers.head, BlockBody(Nil, Nil))))
        setBlockByNumber(2, Some(Block(headers(1), BlockBody(Nil, Nil))))
        setBlockByNumber(9, None)

        ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
        assert(oldBlocks.map(_.header.number) == List[BigInt](3, 4, 5, 6, 7, 8))
      }

    "correctly import block with ommers and ancestor in block queue " in new OmmersTestSetup {
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

      ledger.setExecutionResult(newBlock2, Right(Nil))
      ledger.setExecutionResult(newBlock3WithOmmer, Right(receipts))

      whenReady(ledger.importBlock(newBlock2)){ result => result shouldEqual BlockEnqueued}

      whenReady(ledger.importBlock(newBlock3WithOmmer)){ result => result shouldEqual
        ChainReorganised(List(oldBlock2, oldBlock3), List(newBlock2, newBlock3WithOmmer), List(newTd2, newTd3))
      }

      blockchain.getBestBlock() shouldEqual newBlock3WithOmmer
    }
  }
}
