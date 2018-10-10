package io.iohk.ethereum.ledger

import akka.util.ByteString
import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.{ Block, BlockHeader }
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ Matchers, WordSpec }

class BranchResolutionSpec extends WordSpec with Matchers with ObjectGenerators with ScalaFutures {

  "BranchResolution" should {

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

    "report an invalid branch when headers do not form a chain" in new BranchResolutionTestSetup {
      val headers: List[BlockHeader] = getChainHeaders(1, 10).reverse
      ledger.resolveBranch(headers) shouldEqual InvalidBranch
    }

    // scalastyle:off magic.number
    "report an invalid branch when headers do not reach the current best block number" in new BranchResolutionTestSetup {
      val headers: List[BlockHeader] = getChainHeaders(1, 10)
      setBestBlockNumber(11)

      ledger.resolveBranch(headers) shouldEqual InvalidBranch
    }

    "report an unknown branch in the parent of the first header is unknown" in new BranchResolutionTestSetup {
      val headers: List[BlockHeader] = getChainHeaders(5, 10)

      setGenesisHeader(genesisHeader) // Check genesis block
      setBestBlockNumber(10)
      setHeaderByHash(headers.head.parentHash, None)

      ledger.resolveBranch(headers) shouldEqual UnknownBranch
    }

    "report new better branch found when headers form a branch of higher difficulty than corresponding know headers" in
      new BranchResolutionTestSetup {
        val headers: List[BlockHeader] = getChainHeaders(1, 10)

        setGenesisHeader(genesisHeader) // Check genesis block

        setBestBlockNumber(10)
        setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

        val oldBlocks: List[Block] = headers.map(h => getBlock(h.number, h.difficulty - 1))
        oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

        ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
      }

    "report no need for a chain switch the headers do not have difficulty greater than currently known branch" in
      new BranchResolutionTestSetup {
        val headers: List[BlockHeader] = getChainHeaders(1, 10)

        setGenesisHeader(genesisHeader) // Check genesis block

        setBestBlockNumber(10)
        setHeaderByHash(headers.head.parentHash, Some(getBlock(0).header))

        val oldBlocks: List[Block] = headers.map(h => getBlock(h.number, h.difficulty))
        oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

        ledger.resolveBranch(headers) shouldEqual NoChainSwitch
      }

    "correctly handle a branch that goes up to the genesis block" in new BranchResolutionTestSetup {
      val headers: List[BlockHeader] = genesisHeader :: getChainHeaders(1, 10, genesisHeader.hash)

      setGenesisHeader(genesisHeader)
      setBestBlockNumber(10)
      val oldBlocks: List[Block] = headers.tail.map(h => getBlock(h.number, h.difficulty - 1))
      oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))
      setBlockByNumber(0, Some(Block(genesisHeader, BlockBody(Nil, Nil))))

      ledger.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
    }

    "report an unknown branch if the included genesis header is different than ours" in new BranchResolutionTestSetup {
      val differentGenesis: BlockHeader = genesisHeader.copy(extraData = ByteString("I'm different ;("))
      val headers: List[BlockHeader] = differentGenesis :: getChainHeaders(1, 10, differentGenesis.hash)

      setGenesisHeader(genesisHeader)
      setBestBlockNumber(10)

      ledger.resolveBranch(headers) shouldEqual UnknownBranch
    }

    "not include common prefix as result when finding a new better branch" in new BranchResolutionTestSetup {
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
  }

  trait BranchResolutionTestSetup extends TestSetupWithVmAndValidators with MockBlockchain

}
