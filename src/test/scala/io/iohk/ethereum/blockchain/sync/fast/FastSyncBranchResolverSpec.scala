package io.iohk.ethereum.blockchain.sync.fast

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers
import org.scalamock.scalatest.MockFactory
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain.BlockHeader
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import io.iohk.ethereum.domain.Block
import akka.util.ByteString
import io.iohk.ethereum.domain.BlockchainImpl

class FastSyncBranchResolverSpec extends AnyWordSpec with Matchers with MockFactory {

  private def dummyBlockHeader(num: BigInt, parentHash: ByteString = ByteString.empty): BlockHeader = {
    val emptyByteString = ByteString.empty
    BlockHeader(
      parentHash,
      emptyByteString,
      emptyByteString,
      emptyByteString,
      emptyByteString,
      emptyByteString,
      emptyByteString,
      1,
      num,
      1,
      1,
      1,
      emptyByteString,
      emptyByteString,
      emptyByteString
    )
  }

  private def dummyHeaderChain(fromBlock: BlockHeader, to: BigInt): List[BlockHeader] = ???

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
      val h100 = dummyBlockHeader(100)
      val h99 = dummyBlockHeader(99)
      val h98 = dummyBlockHeader(98)

      inSequence {
        (mockedBlockchain.getBestBlockNumber _).expects().returning(BigInt(100)).once()
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returning(Some(h100))
        (mockedBlockchain.removeBlock _).expects(h100.hash, false).returning(())
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returning(Some(h99))
        (mockedBlockchain.removeBlock _).expects(h99.hash, false).returning(())
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returning(Some(h98))
        (mockedBlockchain.removeBlock _).expects(h98.hash, false).returning(())
      }

      val resolver = new FastSyncBranchResolver {
        override val blockchain: Blockchain = mockedBlockchain
      }
      resolver.discardBlocksAfter(97)
    }
  }

  "RecentBlocksSearch" must {
    "find highest common block header" when {
      "it's at the very end" in {
        val mockedBlockchain = mock[BlockchainImpl]

        val h95 = dummyBlockHeader(95)
        val h96 = dummyBlockHeader(96, parentHash = h95.hash)
        val h97 = dummyBlockHeader(97, parentHash = h96.hash)
        val h98 = dummyBlockHeader(98, parentHash = h97.hash)
        val h99 = dummyBlockHeader(99, parentHash = h98.hash)
        // *** chains diverged after this block => highest common block number [100] ***
        val h100 = dummyBlockHeader(100, parentHash = h99.hash)
        // ***
        val h101r = dummyBlockHeader(101, parentHash = h100.hash)

        val ourBestBlockHeader = 100
        val ourBlockHeaders = List(h95, h96, h97, h98, h99, h100)
        val remoteBlockHeaders = List(h98, h99, h100, h101r) // blocks we request from the remote peer

        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(Some(h100))

        val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
        assert(recentBlocksSearch.getHighestCommonBlock(remoteBlockHeaders, ourBestBlockHeader) === Some(BigInt(100)))
      }
      "it's at the very beginning" in {
        val mockedBlockchain = mock[BlockchainImpl]

        // *** chains diverged after this block => highest common block number [95] ***
        val h95 = dummyBlockHeader(95)
        // ***
        val h96 = dummyBlockHeader(96, parentHash = h95.hash)
        val h96r = dummyBlockHeader(96, parentHash = h95.hash).copy(nonce = ByteString("foo"))
        val h97 = dummyBlockHeader(97, parentHash = h96.hash)
        val h97r = dummyBlockHeader(97, parentHash = h96r.hash)
        val h98 = dummyBlockHeader(98, parentHash = h97.hash)
        val h98r = dummyBlockHeader(98, parentHash = h97r.hash)
        val h99 = dummyBlockHeader(99, parentHash = h98.hash)
        val h99r = dummyBlockHeader(99, parentHash = h98r.hash)
        val h100 = dummyBlockHeader(100, parentHash = h99.hash)
        val h100r = dummyBlockHeader(100, parentHash = h99r.hash)
        val h101r = dummyBlockHeader(101, parentHash = h100r.hash)

        val ourBestBlockHeader = 100
        val ourBlockHeaders = List(h95, h96, h97, h98, h99, h100)
        val remoteBlockHeaders = List(h96r, h97r, h98r, h99r, h100r, h101r) // blocks we receive from the remote peer

        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(95)).returns(Some(h95))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(96)).returns(Some(h96))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(97)).returns(Some(h97))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returns(Some(h98))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returns(Some(h99))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(Some(h100))

        val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
        assert(recentBlocksSearch.getHighestCommonBlock(remoteBlockHeaders, ourBestBlockHeader) === Some(BigInt(95)))
      }
      "it's somewhere in the middle" in {
        val mockedBlockchain = mock[BlockchainImpl]

        val h95 = dummyBlockHeader(95)
        val h96 = dummyBlockHeader(96, parentHash = h95.hash)
        // *** chains diverged after this block => highest common block number [97] ***
        val h97 = dummyBlockHeader(97, parentHash = h96.hash)
        // ***
        val h98 = dummyBlockHeader(98, parentHash = h97.hash)
        val h98r = dummyBlockHeader(98, parentHash = h97.hash).copy(nonce = ByteString("foo"))
        val h99 = dummyBlockHeader(99, parentHash = h98.hash)
        val h99r = dummyBlockHeader(99, parentHash = h98r.hash)
        val h100 = dummyBlockHeader(100, parentHash = h99.hash)
        val h100r = dummyBlockHeader(100, parentHash = h99r.hash)
        val h101r = dummyBlockHeader(101, parentHash = h100r.hash)

        val ourBestBlockHeader = 100
        val ourBlockHeaders = List(h95, h96, h97, h98, h99, h100)
        val remoteBlockHeaders = List(h96, h97, h98r, h99r, h100r, h101r) // blocks we receive from the remote peer

        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(97)).returns(Some(h97))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returns(Some(h98))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returns(Some(h99))
        (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(Some(h100))

        val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
        assert(recentBlocksSearch.getHighestCommonBlock(remoteBlockHeaders, ourBestBlockHeader) === Some(BigInt(97)))
      }
    }
    "return None if there's no common block header" in {
      val mockedBlockchain = mock[BlockchainImpl]

      val h95 = dummyBlockHeader(95)
      val h96 = dummyBlockHeader(96, parentHash = h95.hash)
      val h96r = dummyBlockHeader(96, parentHash = h95.hash).copy(nonce = ByteString("foo"))
      val h97 = dummyBlockHeader(97, parentHash = h96.hash)
      val h97r = dummyBlockHeader(97, parentHash = h96r.hash)
      val h98 = dummyBlockHeader(98, parentHash = h97.hash)
      val h98r = dummyBlockHeader(98, parentHash = h97r.hash)
      val h99 = dummyBlockHeader(99, parentHash = h98.hash)
      val h99r = dummyBlockHeader(99, parentHash = h98r.hash)
      val h100 = dummyBlockHeader(100, parentHash = h99.hash)
      val h100r = dummyBlockHeader(100, parentHash = h99r.hash)
      val h101r = dummyBlockHeader(101, parentHash = h100r.hash)

      val ourBestBlockHeader = 100
      val ourBlockHeaders = List(h95, h96, h97, h98, h99, h100)
      val remoteBlockHeaders =
        List(
          h97r,
          h98r,
          h99r,
          h100r,
          h101r
        ) // we don't request header [96] so we don't see it would be a child of our h95

      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(96)).returns(Some(h96))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(97)).returns(Some(h97))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(98)).returns(Some(h98))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(99)).returns(Some(h99))
      (mockedBlockchain.getBlockHeaderByNumber _).expects(BigInt(100)).returns(Some(h100))

      val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(mockedBlockchain)
      assert(recentBlocksSearch.getHighestCommonBlock(remoteBlockHeaders, ourBestBlockHeader) === None)
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
      assert(true)
    }
    "continue search" when {
      "no parent/child relationship" in pending
      "parent/child relationship but there could be a higher common block" in pending
    }
    "complete search with no match" in pending
  }

}
