package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.{BlockHelpers, WithActorSystemShutDown}
import io.iohk.ethereum.network.PeerId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.immutable.Queue

class BlockFetcherStateSpec
    extends TestKit(ActorSystem("BlockFetcherStateSpec_System"))
    with AnyWordSpecLike
    with WithActorSystemShutDown
    with Matchers {

  lazy val validators = new MockValidatorsAlwaysSucceed

  private val importer = TestProbe().ref

  private val blocks = BlockHelpers.generateChain(5, BlockHelpers.genesis)

  private val peer = PeerId("foo")

  "BlockFetcherState" when {
    "invalidating blocks" should {
      "not allow to go to negative block number" in {
        val (_, actual) =
          BlockFetcherState.initial(validators.blockValidator, 10).invalidateBlocksFrom(-5, None)

        actual.lastBlock shouldBe 0
      }
    }

    "handling requested blocks" should {
      "clear headers queue if got empty list of blocks" in {
        val headers = blocks.map(_.header)

        val result = BlockFetcherState
          .initial(validators.blockValidator, 0)
          .appendHeaders(headers)
          .map(_.handleRequestedBlocks(List(), peer))

        assert(result.map(_.waitingHeaders) === Right(Queue.empty))
      }

      "enqueue requested blocks" in {

        val result = BlockFetcherState
          .initial(validators.blockValidator, 0)
          .appendHeaders(blocks.map(_.header))
          .map(_.handleRequestedBlocks(blocks, peer))

        assert(result.map(_.waitingHeaders) === Right(Queue.empty))
        blocks.foreach { block =>
          assert(result.map(_.blockProviders(block.number)) === Right(peer))
        }
        assert(result.map(_.waitingHeaders.size) === Right(blocks.size))
      }

      "enqueue requested blocks fails when ready blocks is not forming a sequence with given headers" in {

        val result = BlockFetcherState
          .initial(validators.blockValidator, 0)
          .copy(readyBlocks = Queue(blocks.head))
          .appendHeaders(blocks.map(_.header))
          .map(_.handleRequestedBlocks(blocks, peer))

        assert(result.map(_.waitingHeaders) === Left("Given headers should form a sequence with ready blocks"))
      }
    }
  }
}
