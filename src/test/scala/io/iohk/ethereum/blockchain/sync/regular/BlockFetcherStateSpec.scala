package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.domain.Block
import org.scalatest.{Matchers, WordSpecLike}
import io.iohk.ethereum.Fixtures.Blocks.ValidBlock
import io.iohk.ethereum.network.PeerId

class BlockFetcherStateSpec extends TestKit(ActorSystem()) with WordSpecLike with Matchers {
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

  }
}
