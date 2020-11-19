package io.iohk.ethereum.blockchain.sync.regular

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.Mocks.{MockValidatorsAlwaysSucceed, MockValidatorsFailingOnBlockBodies}
import io.iohk.ethereum.BlockHelpers
import io.iohk.ethereum.Fixtures.{Blocks => FixtureBlocks}
import io.iohk.ethereum.blockchain.sync.PeersClient.BlacklistPeer
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.{InternalLastBlockImport, InvalidateBlocksFrom, PickBlocks}
import io.iohk.ethereum.blockchain.sync.{PeersClient, TestSyncConfig}
import io.iohk.ethereum.domain.{Block, ChainWeight, HeadersSeq}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62.{
  BlockBodies,
  BlockHeaders,
  GetBlockBodies,
  GetBlockHeaders,
  NewBlockHashes
}
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class BlockFetcherSpec extends TestKit(ActorSystem("BlockFetcherSpec_System")) with AnyFreeSpecLike with Matchers {

  "BlockFetcher" - {

    "should not requests headers upon invalidation while a request is already in progress, should resume after response" in new TestSetup {
      startFetcher()

      // First headers request
      val firstGetBlockHeadersRequest =
        GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }

      val firstBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)
      val firstGetBlockHeadersResponse = BlockHeaders(firstBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockHeadersResponse))

      // First bodies request
      val firstGetBlockBodiesRequest = GetBlockBodies(firstBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockBodiesRequest => () }

      val firstGetBlockBodiesResponse = BlockBodies(firstBlocksBatch.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockBodiesResponse))

      triggerFetching()

      // Second headers request with response pending
      val secondGetBlockHeadersRequest = GetBlockHeaders(
        Left(firstBlocksBatch.last.number + 1),
        syncConfig.blockHeadersPerRequest,
        skip = 0,
        reverse = false
      )
      // Save the reference to respond to the ask pattern on fetcher
      val refExpectingReply = peersClient.expectMsgPF() {
        case PeersClient.Request(msg, _, _) if msg == secondGetBlockHeadersRequest => peersClient.lastSender
      }

      // Mark first blocks as invalid, no further request should be done
      blockFetcher ! InvalidateBlocksFrom(1, "")
      peersClient.expectMsgClass(classOf[BlacklistPeer])

      peersClient.expectNoMessage()

      // Respond to the second request should make the fetcher resume with his requests
      val secondBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)
      val secondGetBlockHeadersResponse = BlockHeaders(secondBlocksBatch.map(_.header))
      peersClient.send(refExpectingReply, PeersClient.Response(fakePeer, secondGetBlockHeadersResponse))

      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }
    }

    "should not requests headers upon invalidation while a request is already in progress, should resume after failure in response" in new TestSetup {
      startFetcher()

      // First headers request
      val firstGetBlockHeadersRequest =
        GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }

      val firstBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)
      val firstGetBlockHeadersResponse = BlockHeaders(firstBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockHeadersResponse))

      // First bodies request
      val firstGetBlockBodiesRequest = GetBlockBodies(firstBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockBodiesRequest => () }

      val firstGetBlockBodiesResponse = BlockBodies(firstBlocksBatch.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockBodiesResponse))

      triggerFetching()

      // Second headers request with response pending
      val secondGetBlockHeadersRequest = GetBlockHeaders(
        Left(firstBlocksBatch.last.number + 1),
        syncConfig.blockHeadersPerRequest,
        skip = 0,
        reverse = false
      )
      // Save the reference to respond to the ask pattern on fetcher
      val refExpectingReply = peersClient.expectMsgPF() {
        case PeersClient.Request(msg, _, _) if msg == secondGetBlockHeadersRequest => peersClient.lastSender
      }

      // Mark first blocks as invalid, no further request should be done
      blockFetcher ! InvalidateBlocksFrom(1, "")
      peersClient.expectMsgClass(classOf[BlacklistPeer])

      peersClient.expectNoMessage()

      // Failure of the second request should make the fetcher resume with his requests
      peersClient.send(refExpectingReply, PeersClient.RequestFailed(fakePeer, ""))

      peersClient.expectMsgClass(classOf[BlacklistPeer])
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }
    }

    "should not enqueue requested blocks if the received bodies does not match" in new TestSetup {

      // Important: Here we are forcing the mismatch between request headers and received bodies
      override lazy val validators = new MockValidatorsFailingOnBlockBodies

      startFetcher()

      val getBlockHeadersRequest =
        GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockHeadersRequest => () }

      val chain = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)
      val getBlockHeadersResponse = BlockHeaders(chain.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockHeadersResponse))

      val getBlockBodiesRequest = GetBlockBodies(chain.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest => () }

      // This response will be invalid given we are using a special validator!
      val getBlockBodiesResponse = BlockBodies(chain.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse))

      // Fetcher should blacklist the peer and retry asking for the same bodies
      peersClient.expectMsgClass(classOf[BlacklistPeer])
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest => () }

      // Fetcher should not enqueue any new block
      importer.send(blockFetcher, PickBlocks(syncConfig.blocksBatchSize))
      importer.ignoreMsg({ case BlockImporter.NotOnTop => true })
      importer.expectNoMessage()
    }

    "should be able to handle block bodies received in several parts" in new TestSetup {

      startFetcher()

      val getBlockHeadersRequest =
        GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockHeadersRequest => () }

      val chain = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)

      val getBlockHeadersResponse = BlockHeaders(chain.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockHeadersResponse))

      val getBlockBodiesRequest1 = GetBlockBodies(chain.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest1 => () }

      // It will receive all the requested bodies, but splitted in 2 parts.
      val (subChain1, subChain2) = chain.splitAt(syncConfig.blockBodiesPerRequest / 2)

      val getBlockBodiesResponse1 = BlockBodies(subChain1.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse1))

      val getBlockHeadersRequest2 =
        GetBlockHeaders(Left(chain.last.number + 1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockHeadersRequest2 => () }

      val getBlockBodiesRequest2 = GetBlockBodies(subChain2.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest2 => () }

      val getBlockBodiesResponse2 = BlockBodies(subChain2.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse2))

      peersClient.expectNoMessage()

      // Fetcher should enqueue all the received blocks
      importer.send(blockFetcher, PickBlocks(chain.size))
      importer.ignoreMsg({ case BlockImporter.NotOnTop => true })
      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        blocks.map(_.hash).toList shouldEqual chain.map(_.hash)
      }
    }

    "should stop requesting, without blacklist the peer, in case empty bodies are received" in new TestSetup {

      startFetcher()

      val getBlockHeadersRequest =
        GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockHeadersRequest => () }

      val chain = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)

      val getBlockHeadersResponse = BlockHeaders(chain.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockHeadersResponse))

      val getBlockBodiesRequest1 = GetBlockBodies(chain.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest1 => () }

      // It will receive part of the requested bodies.
      val (subChain1, subChain2) = chain.splitAt(syncConfig.blockBodiesPerRequest / 2)

      val getBlockBodiesResponse1 = BlockBodies(subChain1.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse1))

      val getBlockHeadersRequest2 =
        GetBlockHeaders(Left(chain.last.number + 1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockHeadersRequest2 => () }

      val getBlockBodiesRequest2 = GetBlockBodies(subChain2.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest2 => () }

      // We receive empty bodies instead of the second part
      val getBlockBodiesResponse2 = BlockBodies(List())
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse2))

      peersClient.expectNoMessage()

      // If we try to pick the whole chain we should only receive the first part
      importer.send(blockFetcher, PickBlocks(chain.size))
      importer.ignoreMsg({ case BlockImporter.NotOnTop => true })
      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        blocks.map(_.hash).toList shouldEqual subChain1.map(_.hash)
      }
    }

    "should ensure blocks passed to importer are always forming chain" in new TestSetup {
      startFetcher()

      triggerFetching()

      val firstBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)
      val secondBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)
      val alternativeSecondBlocksBatch =
        BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)

      // Fetcher requests for headers
      val firstGetBlockHeadersRequest =
        GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }

      // Respond first headers request
      val firstGetBlockHeadersResponse = BlockHeaders(firstBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockHeadersResponse))

      // Second headers request with response pending
      val secondGetBlockHeadersRequest = GetBlockHeaders(
        Left(secondBlocksBatch.head.number),
        syncConfig.blockHeadersPerRequest,
        skip = 0,
        reverse = false
      )
      // Save the reference to respond to the ask pattern on fetcher
      val refForAnswerSecondHeaderReq = peersClient.expectMsgPF() {
        case PeersClient.Request(msg, _, _) if msg == secondGetBlockHeadersRequest => peersClient.lastSender
      }

      // First bodies request
      val firstGetBlockBodiesRequest = GetBlockBodies(firstBlocksBatch.map(_.hash))
      val refForAnswerFirstBodiesReq = peersClient.expectMsgPF() {
        case PeersClient.Request(msg, _, _) if msg == firstGetBlockBodiesRequest => peersClient.lastSender
      }

      // Block 16 is mined (we could have reached this stage due to invalidation messages sent to the fetcher)
      val minedBlock = alternativeSecondBlocksBatch.drop(5).head
      val minedBlockNumber = minedBlock.number
      blockFetcher ! InternalLastBlockImport(minedBlockNumber)

      // Answer pending requests: first block bodies request + second block headers request
      val secondGetBlockHeadersResponse = BlockHeaders(secondBlocksBatch.map(_.header))
      peersClient.send(refForAnswerSecondHeaderReq, PeersClient.Response(fakePeer, secondGetBlockHeadersResponse))

      val firstGetBlockBodiesResponse = BlockBodies(firstBlocksBatch.map(_.body))
      peersClient.send(refForAnswerFirstBodiesReq, PeersClient.Response(fakePeer, firstGetBlockBodiesResponse))

      // Third headers request with response pending
      peersClient.expectMsgPF() { case PeersClient.Request(GetBlockHeaders(_, _, _, _), _, _) =>
        peersClient.lastSender
      }

      // Second bodies request
      val refForAnswerSecondBodiesReq = peersClient.expectMsgPF() { case PeersClient.Request(GetBlockBodies(_), _, _) =>
        peersClient.lastSender
      }
      peersClient.send(
        refForAnswerSecondBodiesReq,
        PeersClient.Response(fakePeer, BlockBodies(alternativeSecondBlocksBatch.drop(6).map(_.body)))
      )

      importer.send(blockFetcher, PickBlocks(syncConfig.blocksBatchSize))
      importer.ignoreMsg({ case BlockImporter.NotOnTop => true })
      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        val headers = blocks.map(_.header).toList

        assert(HeadersSeq.areChain(headers))
      }
    }
  }

  trait TestSetup extends TestSyncConfig {
    val time = new VirtualTime

    val peersClient: TestProbe = TestProbe()
    val peerEventBus: TestProbe = TestProbe()
    val importer: TestProbe = TestProbe()
    val regularSync: TestProbe = TestProbe()

    lazy val validators = new MockValidatorsAlwaysSucceed

    override lazy val syncConfig = defaultSyncConfig.copy(
      // Same request size was selected for simplification purposes of the flow
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10,
      // Huge timeout on ask pattern
      peerResponseTimeout = 5.minutes
    )

    val fakePeerActor: TestProbe = TestProbe()
    val fakePeer = Peer(new InetSocketAddress("127.0.0.1", 9000), fakePeerActor.ref, false)

    lazy val blockFetcher = system.actorOf(
      BlockFetcher
        .props(
          peersClient.ref,
          peerEventBus.ref,
          regularSync.ref,
          syncConfig,
          validators.blockValidator,
          time.scheduler
        )
    )

    def startFetcher(): Unit = {
      blockFetcher ! BlockFetcher.Start(importer.ref, 0)

      peerEventBus.expectMsg(
        Subscribe(
          MessageClassifier(
            Set(NewBlock.code63, NewBlock.code64, NewBlockHashes.code, BlockHeaders.code),
            PeerSelector.AllPeers
          )
        )
      )
    }

    // Sending a far away block as a NewBlock message
    // Currently BlockFetcher only downloads first block-headers-per-request blocks without this
    def triggerFetching(): Unit = {
      val farAwayBlockWeight = ChainWeight.totalDifficultyOnly(100000)
      val farAwayBlock = Block(FixtureBlocks.ValidBlock.header.copy(number = 1000), FixtureBlocks.ValidBlock.body)

      blockFetcher ! MessageFromPeer(NewBlock(farAwayBlock, farAwayBlockWeight), fakePeer.id)
    }
  }
}
