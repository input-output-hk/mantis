package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import cats.data.NonEmptyList
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.Fixtures.{Blocks => FixtureBlocks}
import io.iohk.ethereum.Mocks.{MockValidatorsAlwaysSucceed, MockValidatorsFailingOnBlockBodies}
import io.iohk.ethereum.blockchain.sync.PeersClient.BlacklistPeer
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.{InternalLastBlockImport, InvalidateBlocksFrom, PickBlocks}
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

import java.net.InetSocketAddress
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class BlockFetcherSpec
    extends TestKit(ActorSystem("BlockFetcherSpec_System"))
    with AnyFreeSpecLike
    with WithActorSystemShutDown
    with Matchers
    with SecureRandomBuilder {

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
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }
    }

    "should not enqueue requested blocks if the received bodies does not match" in new TestSetup {

      // Important: Here we are forcing the mismatch between request headers and received bodies
      override lazy val validators = new MockValidatorsFailingOnBlockBodies

      startFetcher()

      handleFirstBlockBatch()

      // Fetcher should blacklist the peer and retry asking for the same bodies
      peersClient.expectMsgClass(classOf[BlacklistPeer])
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockBodiesRequest => () }

      // Fetcher should not enqueue any new block
      importer.send(blockFetcher, PickBlocks(syncConfig.blocksBatchSize))
      importer.ignoreMsg({ case BlockImporter.NotOnTop => true })
      importer.expectNoMessage(100.millis)
    }

    "should be able to handle block bodies received in several parts" in new TestSetup {

      startFetcher()

      handleFirstBlockBatchHeaders()

      val getBlockBodiesRequest1 = GetBlockBodies(firstBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest1 => () }

      // It will receive all the requested bodies, but splitted in 2 parts.
      val (subChain1, subChain2) = firstBlocksBatch.splitAt(syncConfig.blockBodiesPerRequest / 2)

      val getBlockBodiesResponse1 = BlockBodies(subChain1.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse1))

      val getBlockBodiesRequest2 = GetBlockBodies(subChain2.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest2 => () }

      val getBlockBodiesResponse2 = BlockBodies(subChain2.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse2))

      // We need to wait a while in order to allow fetcher to process all the blocks
      system.scheduler.scheduleOnce(Timeouts.shortTimeout) {
        // Fetcher should enqueue all the received blocks
        importer.send(blockFetcher, PickBlocks(firstBlocksBatch.size))
      }

      importer.ignoreMsg({ case BlockImporter.NotOnTop => true })
      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        blocks.map(_.hash).toList shouldEqual firstBlocksBatch.map(_.hash)
      }
    }

    "should stop requesting, without blacklist the peer, in case empty bodies are received" in new TestSetup {

      startFetcher()

      handleFirstBlockBatchHeaders()

      val getBlockBodiesRequest1 = GetBlockBodies(firstBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest1 => () }

      // It will receive part of the requested bodies.
      val (subChain1, subChain2) = firstBlocksBatch.splitAt(syncConfig.blockBodiesPerRequest / 2)

      val getBlockBodiesResponse1 = BlockBodies(subChain1.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse1))

      val getBlockBodiesRequest2 = GetBlockBodies(subChain2.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == getBlockBodiesRequest2 => () }

      // We receive empty bodies instead of the second part
      val getBlockBodiesResponse2 = BlockBodies(List())
      peersClient.reply(PeersClient.Response(fakePeer, getBlockBodiesResponse2))

      // If we try to pick the whole chain we should only receive the first part
      importer.send(blockFetcher, PickBlocks(firstBlocksBatch.size))
      importer.ignoreMsg({ case BlockImporter.NotOnTop => true })
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

    "should process checkpoint blocks when checkpoint can fit into ready blocks queue" in new TestSetup {
      startFetcher()

      triggerFetching(20)

      val secondBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)
      val checkpointBlock = (new CheckpointBlockGenerator)
        .generate(
          firstBlocksBatch.last,
          Checkpoint(CheckpointingTestHelpers.createCheckpointSignatures(Seq(crypto.generateKeyPair(secureRandom)), firstBlocksBatch.last.hash))
        )

      handleFirstBlockBatchHeaders()

      // Fetcher second request for headers
      val secondGetBlockHeadersRequest =
        GetBlockHeaders(Left(secondBlocksBatch.head.number), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == secondGetBlockHeadersRequest => () }

      // Respond second headers request
      val secondGetBlockHeadersResponse = BlockHeaders(secondBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, secondGetBlockHeadersResponse))

      handleFirstBlockBatchBodies()

      // Second bodies request
      val secondGetBlockBodiesRequest = GetBlockBodies(secondBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == secondGetBlockBodiesRequest => () }

      // Second bodies response
      val secondGetBlockBodiesResponse = BlockBodies(secondBlocksBatch.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, secondGetBlockBodiesResponse))

      // send old checkpoint block
      blockFetcher ! MessageFromPeer(PV64.NewBlock(checkpointBlock, ChainWeight(checkpointBlock.number, checkpointBlock.header.difficulty)), fakePeer.id)

      importer.send(blockFetcher, PickBlocks(syncConfig.blocksBatchSize * 2))
      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        val headers = blocks.map(_.header).toList

        assert(HeadersSeq.areChain(headers))
        assert(headers.lastOption.contains(checkpointBlock.header))
      }
    }

    "should process checkpoint blocks when checkpoint can fit into waiting headers queue" in new TestSetup {
      startFetcher()

      triggerFetching(20)

      val secondBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, firstBlocksBatch.last)
      val secondBlocksBatchFirstPart = secondBlocksBatch.splitAt(5)._1
      val checkpointBlock = (new CheckpointBlockGenerator)
        .generate(
          secondBlocksBatchFirstPart.last,
          Checkpoint(CheckpointingTestHelpers.createCheckpointSignatures(Seq(crypto.generateKeyPair(secureRandom)), secondBlocksBatchFirstPart.last.hash))
        )

      val alternativeSecondBlocksBatch = secondBlocksBatchFirstPart :+ checkpointBlock

      handleFirstBlockBatchHeaders()

      // Fetcher second request for headers
      val secondGetBlockHeadersRequest =
        GetBlockHeaders(Left(secondBlocksBatch.head.number), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == secondGetBlockHeadersRequest => () }

      // Respond second headers request
      val secondGetBlockHeadersResponse = BlockHeaders(secondBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, secondGetBlockHeadersResponse))

      handleFirstBlockBatchBodies()

      // second bodies request
      val secondGetBlockBodiesRequest = GetBlockBodies(secondBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == secondGetBlockBodiesRequest => ()}

      // send old checkpoint block
      blockFetcher ! MessageFromPeer(PV64.NewBlock(checkpointBlock, ChainWeight(checkpointBlock.number, checkpointBlock.header.difficulty)), fakePeer.id)

      // second bodies response
      val secondGetBlockBodiesResponse = BlockBodies(secondBlocksBatch.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, secondGetBlockBodiesResponse))

      // third bodies request after adding checkpoint into the waiting headers queue
      val thirdGetBlockBodiesRequest = GetBlockBodies(alternativeSecondBlocksBatch.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == thirdGetBlockBodiesRequest => ()}

      // third bodies response
      val thirdGetBlockBodiesResponse = BlockBodies(alternativeSecondBlocksBatch.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, thirdGetBlockBodiesResponse))

      // We need to wait a while in order to allow fetcher to process all the blocks
      system.scheduler.scheduleOnce(Timeouts.shortTimeout) {
        importer.send(blockFetcher, PickBlocks(syncConfig.blocksBatchSize * 2))
      }

      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        val headers = blocks.map(_.header).toList

        assert(HeadersSeq.areChain(headers))
        assert(headers.contains(checkpointBlock.header))
      }
    }

    "should process checkpoint blocks when checkpoint not fit into queues" in new TestSetup {
      startFetcher()

      triggerFetching(10)

      val alternativeBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest / 2, FixtureBlocks.Genesis.block)
      val checkpointBlock = (new CheckpointBlockGenerator)
        .generate(
          alternativeBlocksBatch.last,
          Checkpoint(CheckpointingTestHelpers.createCheckpointSignatures(Seq(crypto.generateKeyPair(secureRandom)), alternativeBlocksBatch.last.hash))
        )

      val alternativeBlocksBatchWithCheckpoint = alternativeBlocksBatch :+ checkpointBlock

      handleFirstBlockBatch()

      // send checkpoint block
      blockFetcher ! MessageFromPeer(PV64.NewBlock(checkpointBlock, ChainWeight(checkpointBlock.number, checkpointBlock.header.difficulty)), fakePeer.id)

      // Fetcher new request for headers
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }

      // Respond first headers request
      val newGetBlockHeadersResponse = BlockHeaders(alternativeBlocksBatchWithCheckpoint.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, newGetBlockHeadersResponse))

      // new bodies request
      val newGetBlockBodiesRequest = GetBlockBodies(alternativeBlocksBatchWithCheckpoint.map(_.hash))
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == newGetBlockBodiesRequest => () }

      // First bodies response
      val newGetBlockBodiesResponse = BlockBodies(alternativeBlocksBatchWithCheckpoint.map(_.body))
      peersClient.reply(PeersClient.Response(fakePeer, newGetBlockBodiesResponse))

      // We need to wait a while in order to allow fetcher to process all the blocks
      system.scheduler.scheduleOnce(Timeouts.shortTimeout) {
        importer.send(blockFetcher, PickBlocks(syncConfig.blocksBatchSize))
      }

      importer.expectMsgPF() { case BlockFetcher.PickedBlocks(blocks) =>
        val headers = blocks.map(_.header).toList

        assert(HeadersSeq.areChain(headers))
        assert(headers.contains(checkpointBlock.header))
      }
    }

    "should inform importer when checkpoint block is older than last block" in new TestSetup {
      startFetcher()

      triggerFetching(10)

      val alternativeBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest / 2, FixtureBlocks.Genesis.block)
      val checkpointBlock = (new CheckpointBlockGenerator)
        .generate(
          alternativeBlocksBatch.last,
          Checkpoint(CheckpointingTestHelpers.createCheckpointSignatures(Seq(crypto.generateKeyPair(secureRandom)), alternativeBlocksBatch.last.hash))
        )

      handleFirstBlockBatch()

      // We need to wait a while in order to allow fetcher to process all the blocks
      system.scheduler.scheduleOnce(Timeouts.shortTimeout) {
        importer.send(blockFetcher, PickBlocks(syncConfig.blocksBatchSize))
      }

      importer.expectMsg(BlockFetcher.PickedBlocks(NonEmptyList(firstBlocksBatch.head, firstBlocksBatch.tail)))

      // send old checkpoint block
      blockFetcher ! MessageFromPeer(PV64.NewBlock(checkpointBlock, ChainWeight(checkpointBlock.number, checkpointBlock.header.difficulty)), fakePeer.id)

      importer.expectMsg(BlockImporter.OnTop)

      importer.expectMsg(BlockImporter.NewCheckpointBlock(checkpointBlock, fakePeer.id))
    }

    "should properly handle a request timeout" in new TestSetup {
      override lazy val syncConfig = defaultSyncConfig.copy(
        // Small timeout on ask pattern for testing it here
        peerResponseTimeout = 1.seconds
      )

      startFetcher()

      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }

      // Request should timeout without any response from the peer
      Thread.sleep((syncConfig.peerResponseTimeout + 2.seconds).toMillis)

      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }
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
      blocksBatchSize = 10,
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
          validators.blockValidator
        )
    )

    def startFetcher(): Unit = {
      blockFetcher ! BlockFetcher.Start(importer.ref, 0)

      peerEventBus.expectMsg(
        Subscribe(
          MessageClassifier(
            Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
            PeerSelector.AllPeers
          )
        )
      )
    }

    // Sending a far away block as a NewBlock message
    // Currently BlockFetcher only downloads first block-headers-per-request blocks without this
    def triggerFetching(startingNumber: BigInt = 1000): Unit = {
      val farAwayBlockTotalDifficulty = 100000
      val farAwayBlock = Block(FixtureBlocks.ValidBlock.header.copy(number = startingNumber), FixtureBlocks.ValidBlock.body)

      blockFetcher ! MessageFromPeer(NewBlock(farAwayBlock, farAwayBlockTotalDifficulty), fakePeer.id)
    }

    val firstBlocksBatch = BlockHelpers.generateChain(syncConfig.blockHeadersPerRequest, FixtureBlocks.Genesis.block)

    // Fetcher request for headers
    val firstGetBlockHeadersRequest =
      GetBlockHeaders(Left(1), syncConfig.blockHeadersPerRequest, skip = 0, reverse = false)

    def handleFirstBlockBatchHeaders() = {
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockHeadersRequest => () }

      // Respond first headers request
      val firstGetBlockHeadersResponse = BlockHeaders(firstBlocksBatch.map(_.header))
      peersClient.reply(PeersClient.Response(fakePeer, firstGetBlockHeadersResponse))
    }

    // First bodies request
    val firstGetBlockBodiesRequest = GetBlockBodies(firstBlocksBatch.map(_.hash))
    def handleFirstBlockBatchBodies() = {
      peersClient.expectMsgPF() { case PeersClient.Request(msg, _, _) if msg == firstGetBlockBodiesRequest => () }

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
