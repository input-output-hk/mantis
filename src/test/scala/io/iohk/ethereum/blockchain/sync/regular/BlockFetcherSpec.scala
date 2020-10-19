package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.Fixtures.{Blocks => FixtureBlocks}
import io.iohk.ethereum.blockchain.sync.PeersClient.BlacklistPeer
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.InvalidateBlocksFrom
import io.iohk.ethereum.blockchain.sync.{PeersClient, TestSyncConfig}
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders, NewBlockHashes}
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

class BlockFetcherSpec extends TestKit(ActorSystem("BlockFetcherSpec_System")) with AnyFreeSpecLike with Matchers with BlockHelpers {

  "BlockFetcher" - {

    "should not requests headers upon invalidation if a request is already in progress" in new TestSetup {
      blockFetcher ! BlockFetcher.Start(importer.ref, 0)

      peerEventBus.expectMsg(Subscribe(MessageClassifier(Set(NewBlock.code, NewBlockHashes.code), PeerSelector.AllPeers)))

      // First headers request
      val firstGetBlockHeadersRequest = GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF(){ case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }

      val firstBlocksBatch = generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)
      val firstGetBlockHeadersResponse = BlockHeaders(firstBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockHeadersResponse))

      // First bodies request
      val firstGetBlockBodiesRequest = GetBlockBodies(firstBlocksBatch.map(_.hash))
      peersClient.expectMsgPF(){ case PeersClient.Request(msg, _, _) if msg == firstGetBlockBodiesRequest => () }

      val firstGetBlockBodiesResponse = BlockBodies(firstBlocksBatch.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockBodiesResponse))

      // Trigger sync
      val farAwayBlockTd = 100000
      val farAwayBlock = Block(FixtureBlocks.ValidBlock.header.copy(number = 1000), FixtureBlocks.ValidBlock.body)

      blockFetcher ! MessageFromPeer(NewBlock(farAwayBlock, farAwayBlockTd), fakePeer.id)

      val secondGetBlockHeadersRequest = GetBlockHeaders(Left(firstBlocksBatch.last.number + 1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF(){ case PeersClient.Request(msg, _, _) if msg == secondGetBlockHeadersRequest => () }

      // Mark first blocks as invalid, no further request should be done
      blockFetcher ! InvalidateBlocksFrom(1, "")
      peersClient.expectMsgClass(classOf[BlacklistPeer])

      peersClient.expectNoMessage()
    }
  }

  trait TestSetup extends TestSyncConfig {
    val time = new VirtualTime

    val peersClient: TestProbe = TestProbe()
    val peerEventBus: TestProbe = TestProbe()
    val importer: TestProbe = TestProbe()

    // Same request size was selected for simplification purposes of the flow
    override lazy val syncConfig = defaultSyncConfig.copy(
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10
    )

    val fakePeerActor: TestProbe = TestProbe()
    val fakePeer = Peer(new InetSocketAddress("127.0.0.1", 9000), fakePeerActor.ref, false)

    val blockFetcher = system.actorOf(
      BlockFetcher
        .props(
          peersClient.ref,
          peerEventBus.ref,
          syncConfig,
          time.scheduler
        )
    )
  }

}
