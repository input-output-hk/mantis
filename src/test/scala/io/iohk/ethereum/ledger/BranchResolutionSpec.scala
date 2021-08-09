package io.iohk.ethereum.ledger

import akka.util.ByteString

import cats.data.NonEmptyList

import org.scalacheck.Gen
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import io.iohk.ethereum.ObjectGenerators
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockBody
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.ChainWeight

class BranchResolutionSpec
    extends AnyWordSpec
    with Matchers
    with ObjectGenerators
    with ScalaFutures
    with ScalaCheckPropertyChecks {

  "BranchResolution" should {

    "check if headers are from chain" in new BlockchainSetup {
      val branchResolution = new BranchResolution(blockchainReader)
      val parent: BlockHeader = defaultBlockHeader.copy(number = 1)
      val child: BlockHeader = defaultBlockHeader.copy(number = 2, parentHash = parent.hash)
      branchResolution.doHeadersFormChain(NonEmptyList.of(parent, child)) shouldBe true
    }

    "check if headers are not from chain" in new BlockchainSetup {
      val branchResolution = new BranchResolution(blockchainReader)
      val parent: BlockHeader = defaultBlockHeader.copy(number = 1)
      val otherParent: BlockHeader = defaultBlockHeader.copy(number = 3)
      val child: BlockHeader = defaultBlockHeader.copy(number = 2, parentHash = parent.hash)
      branchResolution.doHeadersFormChain(NonEmptyList.of(otherParent, child)) shouldBe false
    }

    "report an invalid branch when headers do not form a chain" in new BranchResolutionTestSetup {
      val headers = getChainHeadersNel(1, 10).reverse
      branchResolution.resolveBranch(headers) shouldEqual InvalidBranch
    }

    "report an unknown branch in the parent of the first header is unknown" in new BranchResolutionTestSetup {
      val headers = getChainHeadersNel(5, 10)

      setGenesisHeader(genesisHeader) // Check genesis block
      setHeaderInChain(headers.head.parentHash, result = false)

      branchResolution.resolveBranch(headers) shouldEqual UnknownBranch
    }

    "report new better branch found when headers form a branch of higher chain weight than corresponding known headers" in
      new BranchResolutionTestSetup {
        val headers = getChainHeadersNel(1, 10)

        setBestBlockNumber(10)
        setHeaderInChain(headers.head.parentHash)
        setChainWeightByHash(headers.head.parentHash, ChainWeight.zero)

        val oldBlocks = getChain(1, 10, headers.head.parentHash, headers.head.difficulty - 1)
        oldBlocks.map(b => setBlockByNumber(b.header.number, Some(b)))

        branchResolution.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
      }

    "report no need for a chain switch the headers do not have chain weight greater than currently known branch" in
      new BranchResolutionTestSetup {
        val headers = getChainHeadersNel(1, 10)

        setBestBlockNumber(10)
        setHeaderInChain(headers.head.parentHash)
        setChainWeightByHash(headers.head.parentHash, ChainWeight.zero)

        val oldBlocks = getChain(1, 10, headers.head.parentHash, headers.head.difficulty)
        oldBlocks.map(b => setBlockByNumber(b.header.number, Some(b)))

        branchResolution.resolveBranch(headers) shouldEqual NoChainSwitch
      }

    "correctly handle a branch that goes up to the genesis block" in new BranchResolutionTestSetup {
      val headers = genesisHeader :: getChainHeadersNel(1, 10, genesisHeader.hash)

      setHeaderInChain(genesisHeader.parentHash, result = false)
      setGenesisHeader(genesisHeader)
      setBestBlockNumber(10)
      setChainWeightByHash(genesisHeader.hash, ChainWeight.zero)
      setBlockByNumber(0, Some(Block(genesisHeader, BlockBody(Nil, Nil))))

      val oldBlocks: List[Block] = getChain(1, 10, genesisHeader.hash, headers.tail.head.difficulty - 1)
      oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

      branchResolution.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
    }

    "report an unknown branch if the included genesis header is different than ours" in new BranchResolutionTestSetup {
      val differentGenesis: BlockHeader = genesisHeader.copy(extraData = ByteString("I'm different ;("))
      val headers = differentGenesis :: getChainHeadersNel(1, 10, differentGenesis.hash)

      setHeaderInChain(differentGenesis.parentHash, result = false)
      setGenesisHeader(genesisHeader)
      setBestBlockNumber(10)

      branchResolution.resolveBranch(headers) shouldEqual UnknownBranch
    }

    "not include common prefix as result when finding a new better branch" in new BranchResolutionTestSetup {
      val headers = getChainHeadersNel(1, 10)
      val commonParent = headers.toList(1)

      setBestBlockNumber(8)
      setHeaderInChain(headers.head.parentHash)
      setChainWeightByHash(commonParent.hash, ChainWeight.zero)

      val oldBlocks = getChain(3, 8, commonParent.hash)
      oldBlocks.foreach(b => setBlockByNumber(b.header.number, Some(b)))

      setBlockByNumber(1, Some(Block(headers.head, BlockBody(Nil, Nil))))
      setBlockByNumber(2, Some(Block(headers.tail.head, BlockBody(Nil, Nil))))

      branchResolution.resolveBranch(headers) shouldEqual NewBetterBranch(oldBlocks)
      assert(oldBlocks.map(_.header.number) == List[BigInt](3, 4, 5, 6, 7, 8))
    }

    "report a new better branch with higher chain weight even if its shorter than the current " in new BranchResolutionTestSetup {
      val commonParent = getBlock(1, parent = genesisHeader.hash)
      val parentWeight = ChainWeight.zero.increase(commonParent.header)
      val longerBranchLowerWeight = getChain(2, 10, commonParent.hash, difficulty = 100)
      val shorterBranchHigherWeight = getChainNel(2, 8, commonParent.hash, difficulty = 200)

      setHeaderInChain(commonParent.hash)
      setChainWeightForBlock(commonParent, parentWeight)
      setBestBlockNumber(longerBranchLowerWeight.last.number)
      longerBranchLowerWeight.foreach(b => setBlockByNumber(b.number, Some(b)))

      branchResolution.resolveBranch(shorterBranchHigherWeight.map(_.header)) shouldEqual NewBetterBranch(
        longerBranchLowerWeight
      )
    }

    "report a new better branch with a checkpoint" in
      new BranchResolutionTestSetup with CheckpointHelpers {

        val checkpointBranchLength = 5
        // test checkpoint at random position in the chain
        forAll(Gen.choose(2, checkpointBranchLength)) { checkpointPos =>
          val commonParent = getBlock(1, parent = genesisHeader.hash)
          val parentWeight = ChainWeight.zero.increase(commonParent.header)
          val checkpointBranch = NonEmptyList.fromListUnsafe {
            val beforeCheckpoint = commonParent :: getChain(2, checkpointPos - 1, commonParent.hash)
            val checkpoint = getCheckpointBlock(beforeCheckpoint.last)
            val afterCheckpoint = getChain(checkpointPos + 1, checkpointBranchLength, checkpoint.hash)
            beforeCheckpoint.tail ::: checkpoint :: afterCheckpoint
          }

          val noCheckpointBranch = getChain(2, checkpointBranchLength + 2, commonParent.hash)

          setHeaderInChain(commonParent.hash)
          setChainWeightForBlock(commonParent, parentWeight)
          setBestBlockNumber(noCheckpointBranch.last.number)
          noCheckpointBranch.foreach(b => setBlockByNumber(b.number, Some(b)))

          branchResolution.resolveBranch(checkpointBranch.map(_.header)) shouldEqual NewBetterBranch(noCheckpointBranch)
        }
      }

    "report no chain switch when the old branch has a checkpoint and the new one does not" in
      new BranchResolutionTestSetup with CheckpointHelpers {

        val checkpointBranchLength = 5
        // test checkpoint at random position in the chain
        forAll(Gen.choose(2, checkpointBranchLength)) { checkpointPos =>
          val commonParent = getBlock(1, parent = genesisHeader.hash)
          val parentWeight = ChainWeight.zero.increase(commonParent.header)
          val checkpointBranch = NonEmptyList.fromListUnsafe {
            val beforeCheckpoint = commonParent :: getChain(2, checkpointPos - 1, commonParent.hash)
            val checkpoint = getCheckpointBlock(beforeCheckpoint.last)
            val afterCheckpoint = getChain(checkpointPos + 1, checkpointBranchLength, checkpoint.hash)
            beforeCheckpoint.tail ::: checkpoint :: afterCheckpoint
          }

          val noCheckpointBranch = getChainNel(2, checkpointBranchLength + 2, commonParent.hash)

          setHeaderInChain(commonParent.hash)
          setChainWeightForBlock(commonParent, parentWeight)
          setBestBlockNumber(checkpointBranch.last.number)
          checkpointBranch.map(b => setBlockByNumber(b.number, Some(b)))

          branchResolution.resolveBranch(noCheckpointBranch.map(_.header)) shouldEqual NoChainSwitch
        }
      }
  }

  trait BranchResolutionTestSetup extends TestSetupWithVmAndValidators with MockBlockchain {
    val branchResolution = new BranchResolution(blockchainReader)
  }

}
