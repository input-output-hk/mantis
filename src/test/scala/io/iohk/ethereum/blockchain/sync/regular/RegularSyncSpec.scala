package io.iohk.ethereum.blockchain.sync.regular

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestActor.AutoPilot
import akka.testkit.TestKit
import akka.util.ByteString
import cats.data.NonEmptyList
import cats.effect.Resource
import cats.syntax.traverse._
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status
import io.iohk.ethereum.blockchain.sync.SyncProtocol.Status.Progress
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.blockchain.sync.{PeersClient, SyncProtocol}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain.BlockHeaderImplicits._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie.MissingNodeException
import io.iohk.ethereum.network.EtcPeerManagerActor.{GetHandshakedPeers, HandshakedPeers, PeerInfo}
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe}
import io.iohk.ethereum.network.p2p.messages.{Codes, CommonMessages, ProtocolVersions}
import io.iohk.ethereum.network.p2p.messages.PV62._
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.p2p.messages.PV164.NewBlock
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerEventBusActor}
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.{BlockHelpers, ObjectGenerators, ResourceFixtures, WordSpecBase}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalamock.scalatest.AsyncMockFactory
import org.scalatest.diagrams.Diagrams
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, BeforeAndAfterEach}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

class RegularSyncSpec
    extends WordSpecBase
    with ResourceFixtures
    with BeforeAndAfterEach
    with Matchers
    with AsyncMockFactory
    with Diagrams
    with RegularSyncFixtures {
  type Fixture = RegularSyncFixture

  val actorSystemResource =
    Resource.make(Task { ActorSystem() })(system => Task { TestKit.shutdownActorSystem(system) })
  val fixtureResource = actorSystemResource.map(new Fixture(_))

  // Used only in sync tests
  var testSystem: ActorSystem = _
  override def beforeEach(): Unit =
    testSystem = ActorSystem()
  override def afterEach(): Unit =
    TestKit.shutdownActorSystem(testSystem)

  def sync[T <: Fixture](test: => T): Future[Assertion] =
    Future {
      test
      succeed
    }

  "Regular Sync" when {
    "initializing" should {
      "subscribe for new blocks, new hashes and new block headers" in sync(new Fixture(testSystem) {
        regularSync ! SyncProtocol.Start

        peerEventBus.expectMsg(
          PeerEventBusActor.Subscribe(
            MessageClassifier(
              Set(Codes.NewBlockCode, Codes.NewBlockHashesCode, Codes.BlockHeadersCode),
              PeerSelector.AllPeers
            )
          )
        )
      })

      "subscribe to handshaked peers list" in sync(new Fixture(testSystem) {
        regularSync //unlazy
        etcPeerManager.expectMsg(EtcPeerManagerActor.GetHandshakedPeers)
      })
    }

    "fetching blocks" should {
      "fetch headers and bodies concurrently" in sync(new Fixture(testSystem) {
        regularSync ! SyncProtocol.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        // It's weird that we're using block number for total difficulty but I'm too scared to fight this dragon
        peerEventBus.reply(
          MessageFromPeer(NewBlock(testBlocks.last, ChainWeight(0, testBlocks.last.number)), defaultPeer.id)
        )

        peersClient.expectMsgEq(blockHeadersChunkRequest(0))
        peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers)))
        peersClient.expectMsgAllOfEq(
          blockHeadersChunkRequest(1),
          PeersClient.Request.create(GetBlockBodies(testBlocksChunked.head.hashes), PeersClient.BestPeer)
        )
      })

      "blacklist peer which caused failed request" in sync(new Fixture(testSystem) {
        regularSync ! SyncProtocol.Start

        peersClient.expectMsgType[PeersClient.Request[GetBlockHeaders]]
        peersClient.reply(
          PeersClient.RequestFailed(defaultPeer, BlacklistReason.RegularSyncRequestFailed("a random reason"))
        )
        peersClient.expectMsg(
          PeersClient.BlacklistPeer(defaultPeer.id, BlacklistReason.RegularSyncRequestFailed("a random reason"))
        )
      })

      //TODO: To be re-enabled with ETCM-370
      "blacklist peer which returns headers starting from one with higher number than expected" ignore sync(
        new Fixture(
          testSystem
        ) {
          regularSync ! SyncProtocol.Start

          peersClient.expectMsgEq(blockHeadersChunkRequest(0))
          peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked(1).headers)))
          peersClient.expectMsgPF() {
            case PeersClient.BlacklistPeer(id, _) if id == defaultPeer.id => true
          }
        }
      )

      //TODO: To be re-enabled with ETCM-370
      "blacklist peer which returns headers not forming a chain" ignore sync(new Fixture(testSystem) {
        regularSync ! SyncProtocol.Start

        peersClient.expectMsgEq(blockHeadersChunkRequest(0))
        peersClient.reply(
          PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers.filter(_.number % 2 == 0)))
        )
        peersClient.expectMsgPF() {
          case PeersClient.BlacklistPeer(id, _) if id == defaultPeer.id => true
        }
      })

      "wait for time defined in config until issuing a retry request due to no suitable peer" in sync(
        new Fixture(
          testSystem
        ) {
          regularSync ! SyncProtocol.Start

          peersClient.expectMsgEq(blockHeadersChunkRequest(0))
          peersClient.reply(PeersClient.NoSuitablePeer)
          peersClient.expectNoMessage(syncConfig.syncRetryInterval)
          peersClient.expectMsgEq(blockHeadersChunkRequest(0))
        }
      )

      "not fetch new blocks if fetcher's queue reached size defined in configuration" in sync(new Fixture(testSystem) {
        override lazy val syncConfig: SyncConfig = defaultSyncConfig.copy(
          syncRetryInterval = testKitSettings.DefaultTimeout.duration,
          maxFetcherQueueSize = 1,
          blockBodiesPerRequest = 2,
          blockHeadersPerRequest = 2,
          blocksBatchSize = 2
        )

        regularSync ! SyncProtocol.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        peerEventBus.reply(
          MessageFromPeer(NewBlock(testBlocks.last, ChainWeight(0, testBlocks.last.header.difficulty)), defaultPeer.id)
        )

        peersClient.expectMsgEq(blockHeadersChunkRequest(0))
        peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers)))
        peersClient.expectMsgEq(
          PeersClient.Request.create(GetBlockBodies(testBlocksChunked.head.hashes), PeersClient.BestPeer)
        )
        peersClient.reply(PeersClient.Response(defaultPeer, BlockBodies(testBlocksChunked.head.bodies)))

        peersClient.expectNoMessage()
      })
    }

    "resolving branches" should {

      "go back to earlier block in order to find a common parent with new branch" in sync(
        new Fixture(testSystem) with FakeLedger {
          implicit val ec: Scheduler = Scheduler(system.dispatcher)
          override lazy val blockchain: BlockchainImpl = stub[BlockchainImpl]
          (blockchain.getBestBlockNumber _).when().onCall(() => ledger.bestBlock.number)
          override lazy val ledger: TestLedgerImpl = new FakeLedgerImpl()
          override lazy val syncConfig = defaultSyncConfig.copy(
            blockHeadersPerRequest = 5,
            blockBodiesPerRequest = 5,
            blocksBatchSize = 5,
            syncRetryInterval = 1.second,
            printStatusInterval = 0.5.seconds,
            branchResolutionRequestSize = 6
          )

          val commonPart: List[Block] = testBlocks.take(syncConfig.blocksBatchSize)
          val alternativeBranch: List[Block] =
            BlockHelpers.generateChain(syncConfig.blocksBatchSize * 2, commonPart.last)
          val alternativeBlocks: List[Block] = commonPart ++ alternativeBranch

          class BranchResolutionAutoPilot(didResponseWithNewBranch: Boolean, blocks: List[Block])
              extends PeersClientAutoPilot(blocks) {
            override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
              case PeersClient.Request(GetBlockHeaders(Left(nr), maxHeaders, _, _), _, _)
                  if nr >= alternativeBranch.numberAtUnsafe(syncConfig.blocksBatchSize) && !didResponseWithNewBranch =>
                val responseHeaders = alternativeBranch.headers.filter(_.number >= nr).take(maxHeaders.toInt)
                sender ! PeersClient.Response(defaultPeer, BlockHeaders(responseHeaders))
                Some(new BranchResolutionAutoPilot(true, alternativeBlocks))
              case PeersClient.Request(GetBlockBodies(hashes), _, _)
                  if !hashes.toSet.subsetOf(blocks.hashes.toSet) &&
                    hashes.toSet.subsetOf(testBlocks.hashes.toSet) =>
                val matchingBodies = hashes.flatMap(hash => testBlocks.find(_.hash == hash)).map(_.body)
                sender ! PeersClient.Response(defaultPeer, BlockBodies(matchingBodies))
                None
            }
          }

          peersClient.setAutoPilot(new BranchResolutionAutoPilot(didResponseWithNewBranch = false, testBlocks))

          Await.result(ledger.importBlock(BlockHelpers.genesis).runToFuture, remainingOrDefault)

          regularSync ! SyncProtocol.Start

          peerEventBus.expectMsgClass(classOf[Subscribe])
          peerEventBus.reply(
            MessageFromPeer(
              NewBlock(alternativeBlocks.last, ChainWeight(0, alternativeBlocks.last.number)),
              defaultPeer.id
            )
          )
          awaitCond(ledger.bestBlock == alternativeBlocks.last, 5.seconds)
        }
      )
    }

    "go back to earlier positive block in order to resolve a fork when branch smaller than branch resolution size" in sync(
      new Fixture(testSystem) with FakeLedger {
        implicit val ec: Scheduler = Scheduler(system.dispatcher)
        override lazy val blockchain: BlockchainImpl = stub[BlockchainImpl]
        (blockchain.getBestBlockNumber _).when().onCall(() => ledger.bestBlock.number)
        override lazy val ledger: TestLedgerImpl = new FakeLedgerImpl()
        override lazy val syncConfig = defaultSyncConfig.copy(
          syncRetryInterval = 1.second,
          printStatusInterval = 0.5.seconds,
          branchResolutionRequestSize = 12, // Over the original branch size

          // Big so that they don't impact the test
          blockHeadersPerRequest = 50,
          blockBodiesPerRequest = 50,
          blocksBatchSize = 50
        )

        val originalBranch = BlockHelpers.generateChain(10, BlockHelpers.genesis)
        val betterBranch = BlockHelpers.generateChain(originalBranch.size * 2, BlockHelpers.genesis)

        class ForkingAutoPilot(blocksToRespond: List[Block], forkedBlocks: Option[List[Block]])
            extends PeersClientAutoPilot(blocksToRespond) {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case req @ PeersClient.Request(GetBlockBodies(hashes), _, _) =>
              val defaultResult = defaultHandlers(sender)(req)
              if (forkedBlocks.nonEmpty && hashes.contains(blocksToRespond.last.hash)) {
                Some(new ForkingAutoPilot(forkedBlocks.get, None))
              } else
                defaultResult
          }
        }

        peersClient.setAutoPilot(new ForkingAutoPilot(originalBranch, Some(betterBranch)))

        Await.result(ledger.importBlock(BlockHelpers.genesis).runToFuture, remainingOrDefault)

        regularSync ! SyncProtocol.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        val blockFetcher = peerEventBus.sender()
        peerEventBus.reply(
          MessageFromPeer(NewBlock(originalBranch.last, ChainWeight(0, originalBranch.last.number)), defaultPeer.id)
        )

        awaitCond(ledger.bestBlock == originalBranch.last, 5.seconds)

        // As node will be on top, we have to re-trigger the fetching process by simulating a block from the fork being broadcasted
        blockFetcher ! MessageFromPeer(
          NewBlock(betterBranch.last, ChainWeight(0, betterBranch.last.number)),
          defaultPeer.id
        )
        awaitCond(ledger.bestBlock == betterBranch.last, 5.seconds)
      }
    )

    "fetching state node" should {
      abstract class MissingStateNodeFixture(system: ActorSystem) extends Fixture(system) {
        val failingBlock: Block = testBlocksChunked.head.head
        ledger.setImportResult(
          failingBlock,
          Task.now(BlockImportFailedDueToMissingNode(new MissingNodeException(failingBlock.hash)))
        )
      }

      "blacklist peer which returns empty response" in sync(new MissingStateNodeFixture(testSystem) {
        val failingPeer: Peer = peerByNumber(1)

        peersClient.setAutoPilot(new PeersClientAutoPilot {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case PeersClient.Request(GetNodeData(_), _, _) =>
              sender ! PeersClient.Response(failingPeer, NodeData(Nil))
              None
          }
        })

        regularSync ! SyncProtocol.Start

        fishForBlacklistPeer(failingPeer)
      })

      "blacklist peer which returns invalid node" in sync(new MissingStateNodeFixture(testSystem) {
        val failingPeer: Peer = peerByNumber(1)
        peersClient.setAutoPilot(new PeersClientAutoPilot {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case PeersClient.Request(GetNodeData(_), _, _) =>
              sender ! PeersClient.Response(failingPeer, NodeData(List(ByteString("foo"))))
              None
          }
        })

        regularSync ! SyncProtocol.Start

        fishForBlacklistPeer(failingPeer)
      })

      "retry fetching node if validation failed" in sync(new MissingStateNodeFixture(testSystem) {
        def fishForFailingBlockNodeRequest(): Boolean = peersClient.fishForSpecificMessage() {
          case PeersClient.Request(GetNodeData(hash :: Nil), _, _) if hash == failingBlock.hash => true
        }

        class WrongNodeDataPeersClientAutoPilot(var handledRequests: Int = 0) extends PeersClientAutoPilot {
          override def overrides(sender: ActorRef): PartialFunction[Any, Option[AutoPilot]] = {
            case PeersClient.Request(GetNodeData(_), _, _) =>
              val response = handledRequests match {
                case 0 => Some(PeersClient.Response(peerByNumber(1), NodeData(Nil)))
                case 1 => Some(PeersClient.Response(peerByNumber(2), NodeData(List(ByteString("foo")))))
                case _ => None
              }

              response.foreach(sender ! _)
              Some(new WrongNodeDataPeersClientAutoPilot(handledRequests + 1))
          }
        }

        peersClient.setAutoPilot(new WrongNodeDataPeersClientAutoPilot())

        regularSync ! SyncProtocol.Start

        fishForFailingBlockNodeRequest()
        fishForFailingBlockNodeRequest()
        fishForFailingBlockNodeRequest()
      })

      "save fetched node" in sync(new Fixture(testSystem) {
        override lazy val blockchain: BlockchainImpl = stub[BlockchainImpl]
        override lazy val ledger: TestLedgerImpl = stub[TestLedgerImpl]
        val failingBlock: Block = testBlocksChunked.head.head
        peersClient.setAutoPilot(new PeersClientAutoPilot)
        (ledger.resolveBranch _).when(*).returns(NewBetterBranch(Nil)).repeat(10)
        (ledger
          .importBlock(_: Block)(_: Scheduler))
          .when(*, *)
          .returns(Task.now(BlockImportFailedDueToMissingNode(new MissingNodeException(failingBlock.hash))))

        var saveNodeWasCalled: Boolean = false
        val nodeData = List(ByteString(failingBlock.header.toBytes: Array[Byte]))
        (blockchain.getBestBlockNumber _).when().returns(0)
        (blockchain.getBlockHeaderByNumber _).when(*).returns(Some(BlockHelpers.genesis.header))
        (blockchain.saveNode _)
          .when(*, *, *)
          .onCall((hash, encoded, totalDifficulty) => {
            val expectedNode = nodeData.head

            hash should be(kec256(expectedNode))
            encoded should be(expectedNode.toArray)
            totalDifficulty should be(failingBlock.number)

            saveNodeWasCalled = true
          })

        regularSync ! SyncProtocol.Start

        awaitCond(saveNodeWasCalled)
      })
    }

    "catching the top" should {
      "ignore new blocks if they are too new" in sync(new Fixture(testSystem) {
        override lazy val ledger: TestLedgerImpl = stub[TestLedgerImpl]

        val newBlock: Block = testBlocks.last

        regularSync ! SyncProtocol.Start
        peerEventBus.expectMsgClass(classOf[Subscribe])

        peerEventBus.reply(MessageFromPeer(NewBlock(newBlock, ChainWeight(0, 1)), defaultPeer.id))

        Thread.sleep(remainingOrDefault.toMillis)

        (ledger.importBlock(_: Block)(_: Scheduler)).verify(*, *).never()
      })

      "retry fetch of block that failed to import" in sync(new Fixture(testSystem) {
        val failingBlock: Block = testBlocksChunked(1).head

        testBlocksChunked.head.foreach(ledger.setImportResult(_, Task.now(BlockImportedToTop(Nil))))
        ledger.setImportResult(failingBlock, Task.now(BlockImportFailed("test error")))

        peersClient.setAutoPilot(new PeersClientAutoPilot())

        regularSync ! SyncProtocol.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        peerEventBus.reply(
          MessageFromPeer(NewBlock(testBlocks.last, ChainWeight(0, testBlocks.last.number)), defaultPeer.id)
        )

        awaitCond(ledger.didTryToImportBlock(failingBlock))

        peersClient.fishForMsgEq(blockHeadersChunkRequest(1))
      })
    }

    "on top" should {
      "import received new block" in sync(new OnTopFixture(testSystem) {
        goToTop()

        sendNewBlock()

        awaitCond(importedNewBlock)
      })

      "broadcast imported block" in sync(new OnTopFixture(testSystem) {
        goToTop()

        etcPeerManager.expectMsg(GetHandshakedPeers)
        etcPeerManager.reply(HandshakedPeers(handshakedPeers))

        sendNewBlock()

        etcPeerManager.fishForSpecificMessageMatching() {
          case EtcPeerManagerActor.SendMessage(message, _) =>
            message.underlyingMsg match {
              case NewBlock(block, _) if block == newBlock => true
              case _ => false
            }
          case _ => false
        }
      })

      "fetch hashes if received NewHashes message" in sync(new OnTopFixture(testSystem) {
        goToTop()

        blockFetcher !
          MessageFromPeer(NewBlockHashes(List(BlockHash(newBlock.hash, newBlock.number))), defaultPeer.id)

        peersClient.expectMsgPF() { case PeersClient.Request(GetBlockHeaders(_, _, _, _), _, _) =>
          true
        }
      })
    }

    "handling mined blocks" should {
      "not import when importing other blocks" in sync(new Fixture(testSystem) {
        val headPromise: Promise[BlockImportResult] = Promise()
        ledger.setImportResult(testBlocks.head, Task.fromFuture(headPromise.future))
        val minedBlock: Block = BlockHelpers.generateBlock(BlockHelpers.genesis)
        peersClient.setAutoPilot(new PeersClientAutoPilot())

        regularSync ! SyncProtocol.Start

        peerEventBus.expectMsgClass(classOf[Subscribe])
        peerEventBus.reply(
          MessageFromPeer(NewBlock(testBlocks.last, ChainWeight(0, testBlocks.last.number)), defaultPeer.id)
        )

        awaitCond(ledger.didTryToImportBlock(testBlocks.head))
        regularSync ! SyncProtocol.MinedBlock(minedBlock)
        headPromise.success(BlockImportedToTop(Nil))
        Thread.sleep(remainingOrDefault.toMillis)
        ledger.didTryToImportBlock(minedBlock) shouldBe false
      })

      "import when on top" in sync(new OnTopFixture(testSystem) {
        goToTop()

        regularSync ! SyncProtocol.MinedBlock(newBlock)

        awaitCond(importedNewBlock)
      })

      "import when not on top and not importing other blocks" in sync(new Fixture(testSystem) {
        val minedBlock: Block = BlockHelpers.generateBlock(BlockHelpers.genesis)
        ledger.setImportResult(minedBlock, Task.now(BlockImportedToTop(Nil)))

        regularSync ! SyncProtocol.Start

        regularSync ! SyncProtocol.MinedBlock(minedBlock)

        awaitCond(ledger.didTryToImportBlock(minedBlock))
      })

      "broadcast after successful import" in sync(new OnTopFixture(testSystem) {
        goToTop()

        etcPeerManager.expectMsg(GetHandshakedPeers)
        etcPeerManager.reply(HandshakedPeers(handshakedPeers))

        regularSync ! SyncProtocol.MinedBlock(newBlock)

        etcPeerManager.fishForSpecificMessageMatching() {
          case EtcPeerManagerActor.SendMessage(message, _) =>
            message.underlyingMsg match {
              case NewBlock(block, _) if block == newBlock => true
              case _ => false
            }
          case _ => false
        }
      })
    }

    "handling checkpoints" should {
      val checkpoint = ObjectGenerators.fakeCheckpointGen(3, 3).sample.get

      "wait while importing other blocks and then import" in sync(new Fixture(testSystem) {
        val block = testBlocks.head
        val blockPromise: Promise[BlockImportResult] = Promise()
        ledger.setImportResult(block, Task.fromFuture(blockPromise.future))

        ledger.setImportResult(testBlocks(1), Task.now(BlockImportedToTop(Nil)))

        val checkpointBlock = checkpointBlockGenerator.generate(block, checkpoint)
        val newCheckpointMsg = NewCheckpoint(checkpointBlock)
        ledger.setImportResult(checkpointBlock, Task.eval(BlockImportedToTop(Nil)))

        regularSync ! SyncProtocol.Start

        peersClient.setAutoPilot(new PeersClientAutoPilot())

        awaitCond(ledger.didTryToImportBlock(block))
        regularSync ! newCheckpointMsg

        assertForDuration(
          { ledger.didTryToImportBlock(checkpointBlock) shouldBe false },
          1.second
        )
        blockPromise.success(BlockImportedToTop(Nil))
        awaitCond(ledger.didTryToImportBlock(checkpointBlock))
      })

      "import checkpoint when not importing other blocks and broadcast it" in sync(new Fixture(testSystem) {
        regularSync ! SyncProtocol.Start

        val parentBlock = testBlocks.last
        ledger.setImportResult(parentBlock, Task.eval(BlockImportedToTop(Nil)))
        ledger.importBlock(parentBlock)(Scheduler.global)

        val checkpointBlock = checkpointBlockGenerator.generate(parentBlock, checkpoint)
        val newCheckpointMsg = NewCheckpoint(checkpointBlock)
        ledger.setImportResult(
          checkpointBlock,
          // FIXME: lastCheckpointNumber == 0, refactor FakeLedger?
          Task.eval(
            BlockImportedToTop(List(BlockData(checkpointBlock, Nil, ChainWeight(parentBlock.number + 1, 42))))
          )
        )

        etcPeerManager.expectMsg(GetHandshakedPeers)
        etcPeerManager.reply(HandshakedPeers(handshakedPeers))

        regularSync ! newCheckpointMsg

        awaitCond(ledger.didTryToImportBlock(checkpointBlock))
        etcPeerManager.fishForSpecificMessageMatching() {
          case EtcPeerManagerActor.SendMessage(message, _) =>
            message.underlyingMsg match {
              case NewBlock(block, _) if block == checkpointBlock => true
              case _ => false
            }
          case _ => false
        }
      })
    }

    "broadcasting blocks" should {
      "send a NewBlock message without latest checkpoint number when client not support PV164" in sync(
        new OnTopFixture(testSystem) {
          goToTop()

          val peerWithPV63: (Peer, PeerInfo) = {
            val id = peerId(handshakedPeers.size)
            val peer = getPeer(id)
            val peerInfo = getPeerInfo(peer, ProtocolVersions.PV63)
            (peer, peerInfo)
          }

          etcPeerManager.expectMsg(GetHandshakedPeers)
          etcPeerManager.reply(HandshakedPeers(Map(peerWithPV63._1 -> peerWithPV63._2)))

          blockFetcher ! MessageFromPeer(CommonMessages.NewBlock(newBlock, newBlock.number), defaultPeer.id)

          etcPeerManager.fishForSpecificMessageMatching() {
            case EtcPeerManagerActor.SendMessage(message, _) =>
              message.underlyingMsg match {
                case CommonMessages.NewBlock(`newBlock`, _) => true
                case _ => false
              }
            case _ => false
          }
        }
      )

      "send a NewBlock message with latest checkpoint number when client supports PV164" in sync(
        new OnTopFixture(testSystem) {
          goToTop()

          val num: BigInt = 42
          blockchain.saveBestKnownBlocks(num, Some(num))

          etcPeerManager.expectMsg(GetHandshakedPeers)
          etcPeerManager.reply(HandshakedPeers(handshakedPeers))

          blockFetcher ! MessageFromPeer(NewBlock(newBlock, ChainWeight(num, newBlock.number)), defaultPeer.id)

          etcPeerManager.fishForSpecificMessageMatching() {
            case EtcPeerManagerActor.SendMessage(message, _) =>
              message.underlyingMsg match {
                case NewBlock(`newBlock`, _) => true
                case _ => false
              }
            case _ => false
          }
        }
      )
    }

    "reporting progress" should {
      "return NotSyncing until fetching started" in testCaseT { fixture =>
        import fixture._

        for {
          _ <- Task { regularSync ! SyncProtocol.Start }
          before <- getSyncStatus
          _ <- Task {
            peerEventBus.expectMsgClass(classOf[Subscribe])
            peerEventBus.reply(
              MessageFromPeer(
                NewBlock(testBlocks.last, ChainWeight.totalDifficultyOnly(testBlocks.last.number)),
                defaultPeer.id
              )
            )
          }
          after <- getSyncStatus
        } yield {
          assert(before === Status.NotSyncing)
          assert(after === Status.NotSyncing)
        }
      }

      "return initial status after fetching first batch of data" in testCaseT { fixture =>
        import fixture._

        for {
          _ <- testBlocks
            .take(5)
            .traverse(block =>
              Task { blockchain.save(block, Nil, ChainWeight.totalDifficultyOnly(10000), saveAsBestBlock = true) }
            )
          _ <- Task {
            regularSync ! SyncProtocol.Start

            peerEventBus.expectMsgClass(classOf[Subscribe])
            peerEventBus.reply(
              MessageFromPeer(
                NewBlock(testBlocks.last, ChainWeight.totalDifficultyOnly(testBlocks.last.number)),
                defaultPeer.id
              )
            )

            peersClient.expectMsgEq(blockHeadersRequest(6))
            peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers)))
          }
          status <- pollForStatus(_.syncing)
        } yield {
          val lastBlock = testBlocks.last.number
          assert(status === Status.Syncing(5, Progress(5, lastBlock), None))
        }
      }

      "return initial status after fetching first batch of data when starting from genesis" in testCaseT { fixture =>
        import fixture._

        for {
          _ <- Task {
            regularSync ! SyncProtocol.Start

            peerEventBus.expectMsgClass(classOf[Subscribe])
            peerEventBus.reply(
              MessageFromPeer(
                NewBlock(testBlocks.last, ChainWeight.totalDifficultyOnly(testBlocks.last.number)),
                defaultPeer.id
              )
            )

            peersClient.expectMsgEq(blockHeadersChunkRequest(0))
            peersClient.reply(PeersClient.Response(defaultPeer, BlockHeaders(testBlocksChunked.head.headers)))
          }
          status <- pollForStatus(_.syncing)
          lastBlock = testBlocks.last.number
        } yield {
          assert(status === Status.Syncing(0, Progress(0, lastBlock), None))
        }
      }

      "return updated status after importing blocks" in testCaseT { fixture =>
        import fixture._

        for {
          _ <- Task {
            testBlocks.take(6).foreach(ledger.setImportResult(_, Task.eval(BlockImportedToTop(Nil))))

            peersClient.setAutoPilot(new PeersClientAutoPilot(testBlocks.take(6)))

            regularSync ! SyncProtocol.Start

            peerEventBus.expectMsgClass(classOf[Subscribe])
            peerEventBus.reply(
              MessageFromPeer(
                NewBlock(testBlocks.last, ChainWeight.totalDifficultyOnly(testBlocks.last.number)),
                defaultPeer.id
              )
            )
          }
          _ <- ledger.importedBlocks.take(5).lastL
          _ <- fishForStatus {
            case s: Status.Syncing if s.blocksProgress == Progress(5, 20) && s.startingBlockNumber == 0 =>
              s
          }
        } yield succeed
      }

      "return SyncDone when on top" in customTestCaseResourceM(actorSystemResource.map(new OnTopFixture(_))) {
        fixture =>
          import fixture._

          for {
            _ <- Task { goToTop() }
            status <- getSyncStatus
          } yield assert(status === Status.SyncDone)
      }
    }
  }

  trait FakeLedger { self: Fixture =>
    class FakeLedgerImpl extends TestLedgerImpl {
      override def importBlock(
          block: Block
      )(implicit blockExecutionScheduler: Scheduler): Task[BlockImportResult] = {
        val result: BlockImportResult = if (didTryToImportBlock(block)) {
          DuplicateBlock
        } else {
          if (
            importedBlocksSet.isEmpty || bestBlock.isParentOf(block) || importedBlocksSet.exists(_.isParentOf(block))
          ) {
            importedBlocksSet.add(block)
            BlockImportedToTop(List(BlockData(block, Nil, ChainWeight.totalDifficultyOnly(block.header.difficulty))))
          } else if (block.number > bestBlock.number) {
            importedBlocksSet.add(block)
            BlockEnqueued
          } else {
            BlockImportFailed("foo")
          }
        }

        Task.now(result)
      }

      override def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult = {
        val importedHashes = importedBlocksSet.map(_.hash).toSet

        if (
          importedBlocksSet.isEmpty || (importedHashes.contains(
            headers.head.parentHash
          ) && headers.last.number > bestBlock.number)
        )
          NewBetterBranch(Nil)
        else
          UnknownBranch
      }
    }
  }
}
