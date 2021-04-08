package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.{BlockHelpers, Fixtures}
import io.iohk.ethereum.blockchain.sync.fast.BinarySearchSupport._
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolver.SearchState
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain, BlockchainImpl}
import io.iohk.ethereum.network.{Peer, PeerId}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.net.InetSocketAddress

class FastSyncBranchResolverSpec extends AnyWordSpec with Matchers with MockFactory {

  import Fixtures.Blocks.ValidBlock

  private def blocksMap(amount: Int, parent: Block): Map[BigInt, Block] = {
    BlockHelpers.generateChain(amount, parent).map(b => (b.number, b)).toMap
  }

  private def headersMap(amount: Int, parent: Block): Map[BigInt, BlockHeader] = {
    BlockHelpers.generateChain(amount, parent).map(b => (b.number, b.header)).toMap
  }

  private def headersList(blocksMap: Map[BigInt, Block]): List[BlockHeader] =
    blocksMap.values.map(_.header).toList

  "FastSyncBranchResolver" must {
    "calculate childOf block number" in {
      import FastSyncBranchResolver.childOf
      assert(childOf(0) === 1)
      assert(childOf(6) === 7)
    }
    "calculate parentOf block number" in {
      import FastSyncBranchResolver.parentOf
      assert(parentOf(7) === 6)
    }
    "correctly discard all blocks after a certain block number" in {
      val mockedBlockchain = mock[BlockchainImpl]

      val headers = headersMap(amount = 3, parent = Block(ValidBlock.header.copy(number = 97), ValidBlock.body))

      inSequence {
        (mockedBlockchain.getBestBlockNumber _).expects().returning(BigInt(100)).once()
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returning(headers.get(100))
        (mockedBlockchain.removeBlock _).expects(headers(100).hash, false).returning(())
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returning(headers.get(99))
        (mockedBlockchain.removeBlock _).expects(headers(99).hash, false).returning(())
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returning(headers.get(98))
        (mockedBlockchain.removeBlock _).expects(headers(98).hash, false).returning(())
      }

      val resolver = new FastSyncBranchResolver {
        override val blockchain: Blockchain = mockedBlockchain
      }
      resolver.discardBlocksAfter(97)
    }
  }

  "RecentBlocksSearch" must {
    "find highest common block header" when {
      "it's at the very end of the search range" in {
        val mockedBlockchain = mock[BlockchainImpl]

        // our: [..., 97, 98, 99, *100*]
        // peer: [..., 97, 98, 99, *100*, 101, 102]
        val startBlock = Block(ValidBlock.header.copy(number = 97), ValidBlock.body)
        val ourBlocks = blocksMap(amount = 3, parent = startBlock)
        val peerBlocks = ourBlocks ++ blocksMap(amount = 1, parent = ourBlocks(100))

        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(ourBlocks.get(100).map(_.header))

        val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
        assert(
          recentBlocksSearch.getHighestCommonBlock(peerBlocks.values.map(_.header).toList, 100) === Some(BigInt(100))
        )
      }
      "it's at the very beginning of the search range" in {
        val mockedBlockchain = mock[BlockchainImpl]

        val ourBestBlock = 100
        val highestCommonBlock = 97

        // our: [..., *97*, 98, 99, 100]
        // peer: [..., *97*, 98x, 99x, 100, 101x]
        val startBlock = Block(ValidBlock.header.copy(number = 96), ValidBlock.body)
        val ourBlocks = blocksMap(amount = 4, parent = startBlock) // 97, 98, 99, 100
        val peerBlocks = blocksMap(amount = 4, parent = ourBlocks(97)) // 98, 99, 100, 101

        inSequence {
          (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(ourBlocks.get(100).map(_.header))
          (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returns(ourBlocks.get(99).map(_.header))
          (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returns(ourBlocks.get(98).map(_.header))
          (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(97)).returns(ourBlocks.get(97).map(_.header))
        }

        val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
        assert(
          recentBlocksSearch.getHighestCommonBlock(headersList(peerBlocks), ourBestBlock) === Some(
            BigInt(highestCommonBlock)
          )
        )
      }
      "it's somewhere in the middle" in {
        val mockedBlockchain = mock[BlockchainImpl]

        val ourBestBlock = 100
        val highestCommonBlock = 98

        // our: [..., 95, 96, 97, *98*, 99, 100]
        // peer: [..., 95, 96, 97, *98*, 99x, 100x, 101x]
        val startBlock = Block(ValidBlock.header.copy(number = 95), ValidBlock.body)
        val commonBlocks = blocksMap(amount = 3, parent = startBlock)
        val ourBlocks = commonBlocks ++ blocksMap(amount = 2, parent = commonBlocks(highestCommonBlock))
        val peerBlocks = blocksMap(amount = 3, parent = commonBlocks(highestCommonBlock))

        inSequence {
          (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(ourBlocks.get(100).map(_.header))
          (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returns(ourBlocks.get(99).map(_.header))
          (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returns(ourBlocks.get(98).map(_.header))
        }

        val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
        assert(recentBlocksSearch.getHighestCommonBlock(headersList(peerBlocks), ourBestBlock) === Some(BigInt(98)))
      }
    }
    "return None if there's no common block header" in {
      val mockedBlockchain = mock[BlockchainImpl]

      val ourBestBlock = 100

      // our: [..., 95, 96, 97, 98, 99, 100]
      // peer: [..., 95x, 96x, 97x, 98x, 99x, 100x]
      val startBlock = Block(ValidBlock.header.copy(number = 95), ValidBlock.body)
      val divergedStartBlock = Block(ValidBlock.header.copy(number = 95, nonce = ByteString("foo")), ValidBlock.body)
      val ourBlocks = blocksMap(amount = 5, parent = startBlock)
      val peerBlocks = blocksMap(amount = 5, parent = divergedStartBlock)

      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(ourBlocks.get(100).map(_.header))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returns(ourBlocks.get(99).map(_.header))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returns(ourBlocks.get(98).map(_.header))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(97)).returns(ourBlocks.get(97).map(_.header))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(96)).returns(ourBlocks.get(96).map(_.header))

      val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
      assert(recentBlocksSearch.getHighestCommonBlock(headersList(peerBlocks), ourBestBlock) === None)
    }
  }

  "BinarySearch" must {
    "determine correct block number to request" in {
      // we are requesting the child block header of the middle block header, so we're expecting (middle + 1)
      assert(BinarySearchSupport.blockHeaderNumberToRequest(3, 7) === 6)
      // if there is no "middle", we take the lower number
      assert(BinarySearchSupport.blockHeaderNumberToRequest(3, 6) === 5)
      assert(BinarySearchSupport.blockHeaderNumberToRequest(3, 4) === 4)
      assert(BinarySearchSupport.blockHeaderNumberToRequest(4, 4) === 5)
    }
    "complete search with highest common block number" in {
      val ourBestBlock = 10
      val highestCommonBlock = 6

      val commonBlocks: List[Block] = BlockHelpers.generateChain(highestCommonBlock, BlockHelpers.genesis)
      val blocksSaved: List[Block] =
        commonBlocks :++ BlockHelpers.generateChain(ourBestBlock - highestCommonBlock, commonBlocks.last)
      val blocksSavedInPeer: List[Block] =
        commonBlocks :++ BlockHelpers.generateChain(ourBestBlock + 1 - highestCommonBlock, commonBlocks.last)

      val dummyPeer = Peer(PeerId("dummyPeer"), new InetSocketAddress("foo", 1), ActorRef.noSender, false, None, 0)

      val initialSearchState = SearchState(1, 10, dummyPeer)
      val ours = blocksSaved.map(b => (b.number, b)).toMap
      val peer = blocksSavedInPeer.map(b => (b.number, b)).toMap

      val req1 = BinarySearchSupport.blockHeaderNumberToRequest(
        initialSearchState.minBlockNumber,
        initialSearchState.maxBlockNumber
      )
      assert(req1 === 6)

      // checking whether [our:5] is the parent of [peer:6]
      // -> yes, so block 5 is common and we continue to look for higher common blocks
      val s1 = BinarySearchSupport.validateBlockHeaders(
        ours(req1 - 1).header,
        peer(req1).header,
        initialSearchState
      ) match {
        case ContinueBinarySearch(searchState) => searchState
        case _ => fail()
      }
      assert(s1 === SearchState(5, 10, dummyPeer))

      val req2 = BinarySearchSupport.blockHeaderNumberToRequest(s1.minBlockNumber, s1.maxBlockNumber)
      assert(req2 === 8)

      // checking whether [our:7] is the parent of [peer:8]
      // -> no, so block 6 is the max highest block and continue searching
      val s2 = BinarySearchSupport.validateBlockHeaders(
        ours(req2 - 1).header,
        peer(req2).header,
        s1
      ) match {
        case ContinueBinarySearch(searchState) => searchState
        case _ => fail()
      }
      assert(s2 === SearchState(5, 6, dummyPeer))

      val req3 = BinarySearchSupport.blockHeaderNumberToRequest(s2.minBlockNumber, s2.maxBlockNumber)
      assert(req3 === 6)

      // checking whether [our:5] is the parent of [peer:6]
      // -> yes, and 5 was already the minimum and 6 the maximum, so the only block that could be potentially better is 6
      // -> so we set both min and max to 6
      val s3 = BinarySearchSupport.validateBlockHeaders(
        ours(req3 - 1).header,
        peer(req3).header,
        s2
      ) match {
        case ContinueBinarySearch(searchState) => searchState
        case _ => fail()
      }
      assert(s3 === SearchState(6, 6, dummyPeer))

      val req4 = BinarySearchSupport.blockHeaderNumberToRequest(s3.minBlockNumber, s3.maxBlockNumber)
      assert(req4 === 7)

      // checking whether [our:6] is the parent of [peer:7]
      // -> yes, so 6 is the final result
      val res = BinarySearchSupport.validateBlockHeaders(
        ours(req4 - 1).header,
        peer(req4).header,
        s3
      ) match {
        case BinarySearchCompleted(highestHeader) => highestHeader
        case _ => fail()
      }
      assert(res === BigInt(6))
    }
    "complete search with no match" in {
      val ourBestBlock = 10

      val blocksSaved: List[Block] = BlockHelpers.generateChain(8, BlockHelpers.genesis)
      val blocksSavedInPeer: List[Block] = BlockHelpers.generateChain(8, BlockHelpers.genesis)

      val dummyPeer = Peer(PeerId("dummyPeer"), new InetSocketAddress("foo", 1), ActorRef.noSender, false, None, 0)

      val initialSearchState = SearchState(1, 8, dummyPeer)
      val ours = blocksSaved.map(b => (b.number, b)).toMap
      val peer = blocksSavedInPeer.map(b => (b.number, b)).toMap

      val req1 = BinarySearchSupport.blockHeaderNumberToRequest(
        initialSearchState.minBlockNumber,
        initialSearchState.maxBlockNumber
      )
      assert(req1 === 5)

      // checking whether [our:4] is the parent of [peer:5]
      // -> no, so block 3 is the potentially best block
      val s1 = BinarySearchSupport.validateBlockHeaders(
        ours(req1 - 1).header,
        peer(req1).header,
        initialSearchState
      ) match {
        case ContinueBinarySearch(searchState) => searchState
        case _ => fail()
      }
      assert(s1 === SearchState(1, 3, dummyPeer))

      val req2 = BinarySearchSupport.blockHeaderNumberToRequest(s1.minBlockNumber, s1.maxBlockNumber)
      assert(req2 === 3)

      // checking whether [our:2] is the parent of [peer:3]
      // -> no, so block 1 is the max highest block and continue searching
      val s2 = BinarySearchSupport.validateBlockHeaders(
        ours(req2 - 1).header,
        peer(req2).header,
        s1
      ) match {
        case ContinueBinarySearch(searchState) => searchState
        case _ => fail()
      }
      assert(s2 === SearchState(1, 1, dummyPeer))

      val req3 = BinarySearchSupport.blockHeaderNumberToRequest(s2.minBlockNumber, s2.maxBlockNumber)
      assert(req3 === 2)

      // checking whether [our:1] is the parent of [peer:2]
      // -> no, that means not even block 1 is common
      val res = BinarySearchSupport.validateBlockHeaders(
        ours(req3 - 1).header,
        peer(req3).header,
        s2
      )

      assert(res === NoCommonBlock)
    }
  }

}
