package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.Fixtures.Blocks.ValidBlock
import io.iohk.ethereum.domain.Block
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
        val headers = BlockHelpers.generateChain(5, BlockHelpers.genesis).map(_.header)
        val peer = PeerId("foo")

        val result = BlockFetcherState
          .initial(TestProbe().ref, 0)
          .appendHeaders(headers)
          .map(_.handleRequestedBlocks(peer, List()))
          .map(_.waitingHeaders)

        assert(result === Right(Queue.empty))
        result.lastBlock shouldEqual 0
      }

      "enqueue requested blocks" in {

        val blocks = BlockHelpers.generateChain(5, BlockHelpers.genesis)
        val peer = PeerId("foo")

        val result = BlockFetcherState
          .initial(TestProbe().ref, 0)
          .appendHeaders(blocks.map(_.header))
          .map(_.handleRequestedBlocks(peer, blocks.map(_.body)))

        assert(result.waitingHeaders === Right(Queue.empty))
        result.lastBlock shouldEqual blocks.lastBlock.number
        blocks.forEach { block =>
          result.blockProviders(block.number) shouldEqual fakePeerId
        }
        result.knownTop shouldEqual blocks.lastBlock.number
      }
    }
  }
}
