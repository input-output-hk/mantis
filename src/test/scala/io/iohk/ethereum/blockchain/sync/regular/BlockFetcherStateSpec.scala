package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.Fixtures.Blocks.ValidBlock
import io.iohk.ethereum.network.PeerId
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

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

    "appending last full block" should {
      "update last block" in {
        val importer = TestProbe().ref
        val currentBestBlock = Block(ValidBlock.header, ValidBlock.body)
        val newBestBlock = Block(ValidBlock.header.copy(number = ValidBlock.header.number + 1), ValidBlock.body)
        val fakePeerId = PeerId("fake")

        val currentState = BlockFetcherState.initial(importer, validators.blockValidator, currentBestBlock.number)
        val newState = currentState.appendNewBlock(newBestBlock, fakePeerId)
        newState.lastBlock shouldEqual newBestBlock.number
        newState.blockProviders(newBestBlock.number) shouldEqual fakePeerId
        newState.knownTop shouldEqual newBestBlock.number
      }
    }
  }
}
