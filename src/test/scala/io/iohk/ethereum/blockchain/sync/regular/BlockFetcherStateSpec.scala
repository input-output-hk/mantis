package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.{Matchers, WordSpecLike}


class BlockFetcherStateSpec extends TestKit(ActorSystem()) with WordSpecLike with Matchers {
  "BlockFetcherState" when {
    "invalidating blocks" should {
      "not allow to go to negative block number" in {
        val importer = TestProbe().ref
        val (_, actual) = BlockFetcherState.initial(importer, 10).invalidateBlocksFrom(-5, None)

        actual.lastBlock shouldBe 0
      }
    }
  }
}
