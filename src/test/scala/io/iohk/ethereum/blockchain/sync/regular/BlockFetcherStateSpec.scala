package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.Fixtures.Blocks.ValidBlock
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.network.PeerId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.collection.immutable.Queue

class BlockFetcherStateSpec extends TestKit(ActorSystem()) with AnyWordSpecLike with Matchers {
  "BlockFetcherState" when {
    "invalidating blocks" should {
      "not allow to go to negative block number" in {
        val importer = TestProbe().ref
        val (_, actual) = BlockFetcherState.initial(importer, 10).invalidateBlocksFrom(-5, None)

        actual.lastBlock shouldBe 0
      }
    }

    "appending last full block" should {
      "update last block" in {
        val importer = TestProbe().ref
        val currentBestBlock = Block(ValidBlock.header, ValidBlock.body)
        val newBestBlock = Block(ValidBlock.header.copy(number = ValidBlock.header.number + 1), ValidBlock.body)
        val fakePeerId = PeerId("fake")

        val currentState = BlockFetcherState.initial(importer, currentBestBlock.number)
        val newState = currentState.appendNewBlock(newBestBlock, fakePeerId)
        newState.lastBlock shouldEqual newBestBlock.number
        newState.blockProviders(newBestBlock.number) shouldEqual fakePeerId
        newState.knownTop shouldEqual newBestBlock.number
      }
    }

    "handling new block bodies" should {
      "clear headers queue if got empty list of bodies" in {
        val headers = BlockHelpers.generateChain(5, BlockHelpers.genesis).map(_.header)
        val peer = PeerId("foo")

        val result = BlockFetcherState
          .initial(TestProbe().ref, 0)
          .appendHeaders(headers)
          .map(_.addBodies(peer, List()))
          .map(_.waitingHeaders)

        assert(result === Right(Queue.empty))
      }
    }
  }
}
