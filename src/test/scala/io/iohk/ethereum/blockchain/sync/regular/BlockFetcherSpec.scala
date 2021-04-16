package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import cats.data.NonEmptyList
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.Fixtures.{Blocks => FixtureBlocks}
import io.iohk.ethereum.Mocks.{MockValidatorsAlwaysSucceed, MockValidatorsFailingOnBlockBodies}
import io.iohk.ethereum.blockchain.sync.PeersClient.{BestPeer, BlacklistPeer, PeersClientMessage, Request}
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.{FetchEvent, InvalidateBlocksFrom, PickBlocks}
import io.iohk.ethereum.blockchain.sync.{PeersClient, TestSyncConfig}
import io.iohk.ethereum.checkpointing.CheckpointingTestHelpers
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.domain.{Block, ChainWeight, Checkpoint, HeadersSeq}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.{Codes, PV64}
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.{BlockHelpers, Timeouts, WithActorSystemShutDown, crypto}
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.matchers.should.Matchers
import akka.actor.typed.scaladsl.adapter._

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.net.InetSocketAddress

import akka.actor.testkit.typed.FishingOutcome.Complete
import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.actor.typed.ActorRef
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.ProgressProtocol

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class BlockFetcherSpec
    extends ScalaTestWithActorTestKit()
    with AnyFreeSpecLike
    with Matchers
    with SecureRandomBuilder {

  val as: ActorSystem = ActorSystem("BlockFetcherSpec_System")

  "BlockFetcher" - {

    "should not requests headers upon invalidation while a request is already in progress, should resume after response" in new TestSetup {
      startFetcher()

      handleFirstBlockBatch()

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

      handleFirstBlockBatch()

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
      peersClient.expectMsgPF() { case PeersClient.Request(`firstGetBlockHeadersRequest`, _, _) => () }
    }

    "should not enqueue requested blocks if the received bodies do not match" in new TestSetup {

      // Important: Here we are forcing the mismatch between request headers and received bodies
      override lazy val validators = new MockValidatorsFailingOnBlockBodies

      startFetcher()

      handleFirstBlockBatch()

      // Fetcher should blacklist the peer
      peersClient.expectMsgPF(){ case PeersClient.Request(_, _, _) => true }
      peersClient.expectMsgPF(){ case PeersClient.Request(_, _, _) => true }
      peersClient.expectMsgClass(classOf[BlacklistPeer])

      // Fetcher should not enqueue any new block
      importer.send(blockFetcher.toClassic, PickBlocks(syncConfig.blocksBatchSize, importer.ref))
      importer.expectNoMessage(100.millis)
    }

    "should be able to handle block bodies received in several parts" in new TestSetup {

      startFetcher()

      handleFirstBlockBatchHeaders()

//      peersClient.expectMsgPF(hint = "hmmm") { case PeersClient.Request(_, _, _) => () }
//      peersClient.expectMsgPF(hint = "hmmm") { case PeersClient.Request(_, _, _) => () }

      val getBlockBodiesRequest1 = GetBlockBodies(firstBlocksBatch.map(_.hash))
      peersClient.fishForMessage(hint = "222222") { case PeersClient.Request(`getBlockBodiesRequest1`, _, _) => true}

      // It will receive all the requested bodies, but splitted in 2 parts.
      val (subChain1, subChain2) = firstBlocksBatch.splitAt(syncConfig.blockBodiesPerRequest / 2)

      val getBlockBodiesResponse1 = BlockBodies(subChain1.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse1))

      val getBlockBodiesRequest2 = GetBlockBodies(subChain2.map(_.hash))
      peersClient.fishForSpecificMessage(hint = "333333") { case PeersClient.Request(`getBlockBodiesRequest2`, _, _) => true}
      val getBlockBodiesResponse2 = BlockBodies(subChain2.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse2))

      // We need to wait a while in order to allow fetcher to process all the blocks
      as.scheduler.scheduleOnce(Timeouts.shortTimeout) {
        // Fetcher should enqueue all the received blocks
        importer.send(blockFetcher.toClassic, PickBlocks(firstBlocksBatch.size, importer.ref))
      }

      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        blocks.map(_.hash).toList shouldEqual firstBlocksBatch.map(_.hash)
      }
    }

    "should stop requesting, without blacklist the peer, in case empty bodies are received" in new TestSetup {

      startFetcher()

      handleFirstBlockBatchHeaders()

      val getBlockBodiesRequest1 = GetBlockBodies(firstBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(`getBlockBodiesRequest1`, _, _) => () }

      // It will receive part of the requested bodies.
      val (subChain1, subChain2) = firstBlocksBatch.splitAt(syncConfig.blockBodiesPerRequest / 2)

      val getBlockBodiesResponse1 = BlockBodies(subChain1.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse1))

      val getBlockBodiesRequest2 = GetBlockBodies(subChain2.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(`getBlockBodiesRequest2`, _, _) => () }

      // We receive empty bodies instead of the second part
      val getBlockBodiesResponse2 = BlockBodies(List())
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse2))

      // If we try to pick the whole chain we should only receive the first part
      importer.send(blockFetcher.toClassic, PickBlocks(firstBlocksBatch.size, importer.ref))
      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        blocks.map(_.hash).toList shouldEqual subChain1.map(_.hash)
      }
    }

    "should ensure blocks passed to importer are always forming chain" in new TestSetup {
      startFetcher()

      triggerFetching()

      val secondBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)
      val alternativeSecondBlocksBatch =
        BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)

      handleFirstBlockBatchHeaders()

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
      val refForAnswerFirstBodiesReq = peersClient.expectMsgPF() {
        case PeersClient.Request(msg, _, _) if msg == firstGetBlockBodiesRequest => peersClient.lastSender
      }

      // Block 16 is mined (we could have reached this stage due to invalidation messages sent to the fetcher)
//      val minedBlock = alternativeSecondBlocksBatch.drop(5).head

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

      importer.send(blockFetcher.toClassic, PickBlocks(syncConfig.blocksBatchSize, importer.ref))
      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        val headers = blocks.map(_.header).toList

        assert(HeadersSeq.areChain(headers))
      }
    }

    "should properly handle a request timeout" in new TestSetup {
      override lazy val syncConfig = defaultSyncConfig.copy(
        // Small timeout on ask pattern for testing it here
        peerResponseTimeout = 1.seconds
      )

      startFetcher()

      peersClient.expectMsgPF() { case PeersClient.Request(`firstGetBlockHeadersRequest`, _, _) => () }

      // Request should timeout without any response from the peer
      Thread.sleep((syncConfig.peerResponseTimeout + 2.seconds).toMillis)

      peersClient.expectMsgPF() { case PeersClient.Request(`firstGetBlockHeadersRequest`, _, _) => () }
    }
  }

  trait TestSetup extends TestSyncConfig {
    val time = new VirtualTime

    val peersClient = TestProbe()(as)
    val peerEventBus = TestProbe()(as)
    val importer = TestProbe()(as)
    val regularSync = TestProbe()(as)

    lazy val validators = new MockValidatorsAlwaysSucceed

    override lazy val syncConfig = defaultSyncConfig.copy(
      // Same request size was selected for simplification purposes of the flow
      blockHeadersPerRequest = 10,
      blockBodiesPerRequest = 10,
      blocksBatchSize = 10,
      // Huge timeout on ask pattern
      peerResponseTimeout = 5.minutes
    )

    val fakePeerActor = createTestProbe[PeersClientMessage]()
    val fakePeer = Peer(new InetSocketAddress("127.0.0.1", 9000), fakePeerActor.ref.toClassic, false)

    val blockFetcher = spawn(
      BlockFetcher(
        peersClient.ref,
        regularSync.ref,
        syncConfig,
        validators.blockValidator))

    def startFetcher(): Unit = {
      blockFetcher ! BlockFetcher.Start(0)
    }

    // Sending a far away block as a NewBlock message
    // Currently BlockFetcher only downloads first block-headers-per-request blocks without this
    def triggerFetching(startingNumber: BigInt = 1000): Unit = {
      val farAwayBlockTotalDifficulty = 100000
      val farAwayBlock =
        Block(FixtureBlocks.ValidBlock.header.copy(number = startingNumber), FixtureBlocks.ValidBlock.body)

//      blockImporter ! MessageFromPeer(NewBlock(farAwayBlock, farAwayBlockTotalDifficulty), fakePeer.id)
    }

    val firstBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)

    // Fetcher request for headers
    val firstGetBlockHeadersRequest: GetBlockHeaders =
      GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)

    def handleFirstBlockBatchHeaders() = {
      peersClient.expectMsgPF() { case PeersClient.Request(`firstGetBlockHeadersRequest`, _, _) => () }

      // Respond first headers request
      val firstGetBlockHeadersResponse = BlockHeaders(firstBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockHeadersResponse))

      val secondGetBlockHeadersRequest: GetBlockHeaders =
        GetBlockHeaders(Left(11), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)

      peersClient.expectMsgPF() { case PeersClient.Request(`secondGetBlockHeadersRequest`, _, _) => () }
      // Respond first headers request
      val secondBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)
      val secondGetBlockHeadersResponse = BlockHeaders(secondBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, secondGetBlockHeadersResponse))
    }

    // First bodies request
    val firstGetBlockBodiesRequest = GetBlockBodies(firstBlocksBatch.map(_.hash))
    def handleFirstBlockBatchBodies() = {
      peersClient.expectMsgPF(hint = "nooowaaay") { case PeersClient.Request(`firstGetBlockBodiesRequest`, _, _) => () }

      // First bodies response
      val firstGetBlockBodiesResponse = BlockBodies(firstBlocksBatch.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockBodiesResponse))
    }

    def handleFirstBlockBatch() = {
      handleFirstBlockBatchHeaders()
      handleFirstBlockBatchBodies()
    }
  }
}
