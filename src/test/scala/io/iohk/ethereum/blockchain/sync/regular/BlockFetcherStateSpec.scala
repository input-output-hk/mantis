package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.{BlockHelpers, WithActorSystemShutDown}
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.utils.ByteStringUtils
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
          BlockFetcherState.initial(importer, validators.blockValidator, 10).invalidateBlocksFrom(-5, None)

        actual.lastBlock shouldBe 0
      }
    }

    "handling requested blocks" should {
      "clear headers queue if got empty list of blocks" in {
        val headers = blocks.map(_.header)

        val result = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(headers)
          .map(_.handleRequestedBlocks(List(), peer))

        assert(result.map(_.waitingHeaders) === Right(Queue.empty))
      }

      "enqueue requested blocks" in {

        val result = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(blocks.map(_.header))
          .map(_.handleRequestedBlocks(blocks, peer))

        assert(result.map(_.waitingHeaders) === Right(Queue.empty))
        blocks.foreach { block =>
          assert(result.map(_.blockProviders(block.number)) === Right(peer))
        }
        assert(result.map(_.knownTop) === Right(blocks.last.number))
      }
    }

    "trying to insert block into the queues" should {
      "insert block into the ready blocks queue" in {
        val (front, _) = blocks.splitAt(2)
        val testBlock = BlockHelpers.generateBlock(front.last)
        val peerId = PeerId("bar")

        val result = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(blocks.map(_.header))
          .map(_.handleRequestedBlocks(blocks, peer))
          .flatMap(_.tryInsertBlock(testBlock, peerId))

        assert(result.map(_.waitingHeaders) === Right(Queue.empty))
        assert(result.map(_.readyBlocks) === Right(Queue.empty.enqueueAll(front :+ testBlock)))
        front.foreach { block =>
          assert(result.map(_.blockProviders(block.number)) === Right(peer))
        }
        assert(result.map(_.blockProviders(testBlock.number)) === Right(peerId))
        assert(result.map(_.knownTop) === Right(testBlock.number))
      }

      "insert block into the waiting headers queue" in {
        val (front, _) = blocks.splitAt(2)
        val testBlock = BlockHelpers.generateBlock(front.last)

        val result = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(blocks.map(_.header))
          .flatMap(_.tryInsertBlock(testBlock, peer))

        assert(result.map(_.readyBlocks) === Right(Queue.empty))
        assert(result.map(_.waitingHeaders) === Right(Queue.empty.enqueueAll((front :+ testBlock).map(_.header))))
        assert(result.map(_.knownTop) === Right(testBlock.number))
      }

      "return state without changes when block is already in the queues" in {
        val (front, _) = blocks.splitAt(2)
        val testBlock = front.last

        val initial = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(blocks.map(_.header))

        val result = initial.flatMap(_.tryInsertBlock(testBlock, peer))

        assert(result === initial)
      }

      "return error msg when cannot insert block" in {
        val testBlock = BlockHelpers.generateBlock(BlockHelpers.genesis)

        val result = BlockFetcherState
          .initial(importer, validators.blockValidator, 0)
          .appendHeaders(blocks.map(_.header))
          .flatMap(_.tryInsertBlock(testBlock, peer))

        assert(result === Left(s"Cannot insert block [${ByteStringUtils.hash2string(testBlock.hash)}] into the queues"))
      }
    }
  }
}
