package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.network.PeerId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.immutable.Queue

class BlockFetcherStateSpec extends TestKit(ActorSystem()) with AnyWordSpecLike with Matchers {

  lazy val validators = new MockValidatorsAlwaysSucceed

  "BlockFetcherState" when {
    "invalidating blocks" should {
      "not allow to go to negative block number" in {
        val importer = TestProbe().ref
        val (_, actual) =
          BlockFetcherState.initial(importer, validators.blockValidator, 10).invalidateBlocksFrom(-5, None)

        actual.lastBlock shouldBe 0
      }
    }

    "handling requested blocks" should {
      "clear headers queue if got empty list of blocks" in {
        val importer = TestProbe().ref
        val headers = BlockHelpers.generateChain(5, BlockHelpers.genesis).map(_.header)
        val peer = PeerId("foo")

        val result = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(headers)
          .map(_.handleRequestedBlocks(List(), peer))

        assert(result.map(_.waitingHeaders) === Right(Queue.empty))
        assert(result.map(_.lastBlock) === Right(headers.last.number))
      }

      "enqueue requested blocks" in {
        val importer = TestProbe().ref
        val blocks = BlockHelpers.generateChain(5, BlockHelpers.genesis)
        val peer = PeerId("foo")

        val result = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(blocks.map(_.header))
          .map(_.handleRequestedBlocks(blocks, peer))

        assert(result.map(_.waitingHeaders) === Right(Queue.empty))
        assert(result.map(_.lastBlock) === Right(blocks.last.number))
        blocks.foreach { block =>
          assert(result.map(_.blockProviders(block.number)) === Right(peer))
        }
        assert(result.map(_.knownTop) === Right(blocks.last.number))
      }
    }
  }
}
