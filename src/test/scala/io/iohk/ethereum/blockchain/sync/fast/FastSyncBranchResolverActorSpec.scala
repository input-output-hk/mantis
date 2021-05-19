package io.iohk.ethereum.blockchain.sync.fast

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.gracefulStop
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestKit, TestProbe}
import akka.util.{ByteString, Timeout}
import cats.effect.concurrent.Deferred
import cats.implicits._
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolverActor.{BranchResolvedSuccessful, StartBranchResolver}
import io.iohk.ethereum.domain.{Block, BlockHeader, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.utils.Logger
import io.iohk.ethereum.{BlockHelpers, NormalPatience, WithActorSystemShutDown}
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.{ReplaySubject, Subject}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpecLike

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Random
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolverActor.BranchResolutionFailed
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolverActor.BranchResolutionFailed.NoCommonBlockFound

class FastSyncBranchResolverActorSpec
    extends TestKit(ActorSystem("FastSyncBranchResolver_testing"))
    with AnyFreeSpecLike
    with ScalaFutures
    with NormalPatience
    with WithActorSystemShutDown { self =>
  implicit val timeout: Timeout = Timeout(30.seconds)

  import FastSyncBranchResolverActorSpec._

  "FastSyncBranchResolver" - {
    "fetch headers from the new master peer" - {
      "the chain is repaired from the first request to the new master pair and then the last two blocks are removed" in new TestSetup {
        override implicit lazy val system = self.system
        override implicit val scheduler = Scheduler(system.dispatcher)

        val sender = TestProbe("sender")

        val commonBlocks: List[Block] = BlockHelpers.generateChain(
          5,
          BlockHelpers.genesis,
          block => block
        )

        val blocksSaved: List[Block] = commonBlocks :++ BlockHelpers.generateChain(
          1,
          commonBlocks.last,
          block => block
        )

        val blocksSavedInPeer: List[Block] = commonBlocks :++ BlockHelpers.generateChain(
          2,
          commonBlocks.last,
          block => block
        )

        val firstBatchBlockHeaders: List[Block] =
          blocksSavedInPeer.slice(blocksSavedInPeer.size - syncConfig.blockHeadersPerRequest, blocksSavedInPeer.size)

        val blocksSentFromPeer: Map[Int, List[Block]] = Map(1 -> firstBatchBlockHeaders)

        saveBlocks(blocksSaved)
        val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSentFromPeer)
        val fastSyncBranchResolver =
          creatFastSyncBranchResolver(sender.ref, etcPeerManager, CacheBasedBlacklist.empty(BlacklistMaxElements))

        val expectation: PartialFunction[Any, BranchResolvedSuccessful] = {
          case r @ BranchResolvedSuccessful(num, _) if num == BigInt(5) => r
        }

        val response = (for {
          _ <- Task(sender.send(fastSyncBranchResolver, StartBranchResolver))
          response <- Task(sender.expectMsgPF()(expectation))
          _ <- Task(stopController(fastSyncBranchResolver))
        } yield response).runSyncUnsafe()
        assert(getBestPeers.contains(response.masterPeer))
      }

      "The chain is repaired doing binary searching with the new master peer and then remove the last invalid blocks" - {
        "highest common block is in the middle" in new TestSetup {
          override implicit lazy val system = self.system
          override implicit val scheduler = Scheduler(system.dispatcher)

          val sender = TestProbe("sender")

          val commonBlocks: List[Block] = BlockHelpers.generateChain(5, BlockHelpers.genesis)
          val blocksSaved: List[Block] = commonBlocks :++ BlockHelpers.generateChain(5, commonBlocks.last)
          val blocksSavedInPeer: List[Block] = commonBlocks :++ BlockHelpers.generateChain(6, commonBlocks.last)

          val firstBatchBlockHeaders =
            blocksSavedInPeer.slice(blocksSavedInPeer.size - syncConfig.blockHeadersPerRequest, blocksSavedInPeer.size)

          val blocksSentFromPeer: Map[Int, List[Block]] = Map(
            1 -> firstBatchBlockHeaders,
            2 -> List(blocksSavedInPeer.get(5).get),
            3 -> List(blocksSavedInPeer.get(7).get),
            4 -> List(blocksSavedInPeer.get(5).get),
            5 -> List(blocksSavedInPeer.get(6).get)
          )

          saveBlocks(blocksSaved)
          val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSentFromPeer)
          val fastSyncBranchResolver =
            creatFastSyncBranchResolver(sender.ref, etcPeerManager, CacheBasedBlacklist.empty(BlacklistMaxElements))

          val expectation: PartialFunction[Any, BranchResolvedSuccessful] = {
            case r @ BranchResolvedSuccessful(num, _) if num == BigInt(5) => r
          }

          val response = (for {
            _ <- Task(sender.send(fastSyncBranchResolver, StartBranchResolver))
            response <- Task(sender.expectMsgPF()(expectation))
            _ <- Task(stopController(fastSyncBranchResolver))
          } yield response).runSyncUnsafe()
          assert(getBestPeers.contains(response.masterPeer))
        }
        "highest common block is in the first half" in new TestSetup {
          override implicit lazy val system = self.system
          override implicit val scheduler = Scheduler(system.dispatcher)

          val sender = TestProbe("sender")

          val commonBlocks: List[Block] = BlockHelpers.generateChain(3, BlockHelpers.genesis)
          val blocksSaved: List[Block] = commonBlocks :++ BlockHelpers.generateChain(7, commonBlocks.last)
          val blocksSavedInPeer: List[Block] = commonBlocks :++ BlockHelpers.generateChain(8, commonBlocks.last)

          val firstBatchBlockHeaders =
            blocksSavedInPeer.slice(blocksSavedInPeer.size - syncConfig.blockHeadersPerRequest, blocksSavedInPeer.size)

          val blocksSentFromPeer: Map[Int, List[Block]] = Map(
            1 -> firstBatchBlockHeaders,
            2 -> List(blocksSavedInPeer.get(5).get),
            3 -> List(blocksSavedInPeer.get(2).get),
            4 -> List(blocksSavedInPeer.get(3).get),
            5 -> List(blocksSavedInPeer.get(3).get),
            6 -> List(blocksSavedInPeer.get(4).get)
          )

          saveBlocks(blocksSaved)
          val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSentFromPeer)
          val fastSyncBranchResolver =
            creatFastSyncBranchResolver(sender.ref, etcPeerManager, CacheBasedBlacklist.empty(BlacklistMaxElements))

          val expectation: PartialFunction[Any, BranchResolvedSuccessful] = {
            case r @ BranchResolvedSuccessful(num, _) if num == BigInt(3) => r
          }

          val response = (for {
            _ <- Task(sender.send(fastSyncBranchResolver, StartBranchResolver))
            response <- Task(sender.expectMsgPF()(expectation))
            _ <- Task(stopController(fastSyncBranchResolver))
          } yield response).runSyncUnsafe()
          assert(getBestPeers.contains(response.masterPeer))
        }

        "highest common block is in the second half" in new TestSetup {
          override implicit lazy val system = self.system
          override implicit val scheduler = Scheduler(system.dispatcher)

          val sender = TestProbe("sender")

          val commonBlocks: List[Block] = BlockHelpers.generateChain(6, BlockHelpers.genesis)
          val blocksSaved: List[Block] = commonBlocks :++ BlockHelpers.generateChain(4, commonBlocks.last)
          val blocksSavedInPeer: List[Block] = commonBlocks :++ BlockHelpers.generateChain(5, commonBlocks.last)

          val firstBatchBlockHeaders =
            blocksSavedInPeer.slice(blocksSavedInPeer.size - syncConfig.blockHeadersPerRequest, blocksSavedInPeer.size)

          val blocksSentFromPeer: Map[Int, List[Block]] = Map(
            1 -> firstBatchBlockHeaders,
            2 -> List(blocksSavedInPeer.get(5).get),
            3 -> List(blocksSavedInPeer.get(7).get),
            4 -> List(blocksSavedInPeer.get(5).get),
            5 -> List(blocksSavedInPeer.get(6).get)
          )

          saveBlocks(blocksSaved)
          val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSentFromPeer)
          val fastSyncBranchResolver =
            creatFastSyncBranchResolver(sender.ref, etcPeerManager, CacheBasedBlacklist.empty(BlacklistMaxElements))

          val expectation: PartialFunction[Any, BranchResolvedSuccessful] = {
            case r @ BranchResolvedSuccessful(num, _) if num == BigInt(6) => r
          }

          val response = (for {
            _ <- Task(sender.send(fastSyncBranchResolver, StartBranchResolver))
            response <- Task(sender.expectMsgPF()(expectation))
            _ <- Task(stopController(fastSyncBranchResolver))
          } yield response).runSyncUnsafe()
          assert(getBestPeers.contains(response.masterPeer))
        }
      }

      "No common block is found" in new TestSetup {
        override implicit lazy val system = self.system
        override implicit val scheduler = Scheduler(system.dispatcher)

        val sender = TestProbe("sender")

        // same genesis block but no common blocks
        val blocksSaved: List[Block] = BlockHelpers.generateChain(5, BlockHelpers.genesis)
        val blocksSavedInPeer: List[Block] = BlockHelpers.generateChain(6, BlockHelpers.genesis)

        val firstBatchBlockHeaders =
          blocksSavedInPeer.slice(blocksSavedInPeer.size - syncConfig.blockHeadersPerRequest, blocksSavedInPeer.size)

        val blocksSentFromPeer: Map[Int, List[Block]] = Map(
          1 -> firstBatchBlockHeaders,
          2 -> List(blocksSavedInPeer.get(3).get),
          3 -> List(blocksSavedInPeer.get(1).get),
          4 -> List(blocksSavedInPeer.get(1).get)
        )

        saveBlocks(blocksSaved)
        val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSentFromPeer)
        val fastSyncBranchResolver =
          creatFastSyncBranchResolver(sender.ref, etcPeerManager, CacheBasedBlacklist.empty(BlacklistMaxElements))

        log.debug(s"*** peers: ${handshakedPeers.map(p => (p._1.id, p._2.maxBlockNumber))}")
        (for {
          _ <- Task(sender.send(fastSyncBranchResolver, StartBranchResolver))
          response <- Task(sender.expectMsg(BranchResolutionFailed(NoCommonBlockFound)))
          _ <- Task(stopController(fastSyncBranchResolver))
        } yield response).runSyncUnsafe()
      }
    }
  }

  trait TestSetup extends EphemBlockchainTestSetup with TestSyncConfig with TestSyncPeers {

    def peerId(number: Int): PeerId = PeerId(s"peer_$number")
    def getPeer(id: PeerId): Peer =
      Peer(id, new InetSocketAddress("127.0.0.1", 0), TestProbe(id.value).ref, incomingConnection = false)
    def getPeerInfo(peer: Peer, protocolVersion: Int = ProtocolVersions.PV164): PeerInfo = {
      val status =
        RemoteStatus(
          protocolVersion,
          1,
          ChainWeight.totalDifficultyOnly(1),
          ByteString(s"${peer.id}_bestHash"),
          ByteString("unused")
        )
      PeerInfo(
        status,
        forkAccepted = true,
        chainWeight = status.chainWeight,
        maxBlockNumber = Random.between(1, 10),
        bestBlockHash = status.bestHash
      )
    }

    val handshakedPeers: Map[Peer, PeerInfo] =
      (0 to 5).toList.map((peerId _).andThen(getPeer)).fproduct(getPeerInfo(_)).toMap

    def saveBlocks(blocks: List[Block]): Unit = {
      blocks.foreach(block => blockchain.save(block, Nil, ChainWeight.totalDifficultyOnly(1), saveAsBestBlock = true))
    }

    def createEtcPeerManager(peers: Map[Peer, PeerInfo], blocks: Map[Int, List[Block]])(implicit
        scheduler: Scheduler
    ): ActorRef = {
      val etcPeerManager = TestProbe("etc_peer_manager")
      val autoPilot =
        new EtcPeerManagerAutoPilot(
          responsesSubject,
          peersConnectedDeferred,
          peers,
          blocks
        )
      etcPeerManager.setAutoPilot(autoPilot)
      etcPeerManager.ref
    }

    def creatFastSyncBranchResolver(fastSync: ActorRef, etcPeerManager: ActorRef, blacklist: Blacklist): ActorRef =
      system.actorOf(
        FastSyncBranchResolverActor.props(
          fastSync = fastSync,
          peerEventBus = TestProbe("peer_event_bus").ref,
          etcPeerManager = etcPeerManager,
          blockchain = blockchain,
          blacklist = blacklist,
          syncConfig = syncConfig,
          appStateStorage = storagesInstance.storages.appStateStorage,
          scheduler = system.scheduler
        )
      )

    def stopController(actorRef: ActorRef): Unit = {
      awaitCond(gracefulStop(actorRef, actorAskTimeout.duration).futureValue)
    }

    def getBestPeers: List[Peer] = {
      val maxBlock = handshakedPeers.toList.map { case (_, peerInfo) => peerInfo.maxBlockNumber }.max
      handshakedPeers.toList.filter { case (_, peerInfo) => peerInfo.maxBlockNumber == maxBlock }.map(_._1)
    }
  }
}

object FastSyncBranchResolverActorSpec extends Logger {

  private val BlacklistMaxElements: Int = 100

  private val responsesSubject: Subject[MessageFromPeer, MessageFromPeer] = ReplaySubject()
  private val peersConnectedDeferred = Deferred.unsafe[Task, Unit]

  var responses: Observable[MessageFromPeer] = responsesSubject

  def fetchedHeaders: Observable[Seq[BlockHeader]] = {
    responses
      .collect { case MessageFromPeer(BlockHeaders(headers), _) =>
        headers
      }
  }

  class EtcPeerManagerAutoPilot(
      responses: Subject[MessageFromPeer, MessageFromPeer],
      peersConnected: Deferred[Task, Unit],
      peers: Map[Peer, PeerInfo],
      blocks: Map[Int, List[Block]]
  )(implicit scheduler: Scheduler)
      extends AutoPilot {

    var blockIndex = 0
    lazy val blocksSetSize = blocks.size

    def run(sender: ActorRef, msg: Any): EtcPeerManagerAutoPilot = {
      msg match {
        case EtcPeerManagerActor.GetHandshakedPeers =>
          sender ! EtcPeerManagerActor.HandshakedPeers(peers)
          peersConnected.complete(()).onErrorHandle(_ => ()).runSyncUnsafe()
        case sendMsg @ EtcPeerManagerActor.SendMessage(rawMsg, peerId) =>
          val response = rawMsg.underlyingMsg match {
            case GetBlockHeaders(_, _, _, false) =>
              if (blockIndex < blocksSetSize)
                blockIndex += 1
              BlockHeaders(blocks.get(blockIndex).map(_.map(_.header)).getOrElse(Nil))
          }
          val theResponse = MessageFromPeer(response, peerId)
          sender ! theResponse
          responses.onNext(theResponse)
          if (blockIndex == blocksSetSize)
            responses.onComplete()
      }
      this
    }
  }
}
