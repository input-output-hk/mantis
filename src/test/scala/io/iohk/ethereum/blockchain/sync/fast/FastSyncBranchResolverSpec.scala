package io.iohk.ethereum.blockchain.sync.fast

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.gracefulStop
import akka.testkit.TestActor.AutoPilot
import akka.testkit.{TestKit, TestProbe}
import akka.util.{ByteString, Timeout}
import cats.effect.concurrent.Deferred
import cats.implicits._
import io.iohk.ethereum.blockchain.sync.PeerListSupport.PeersMap
import io.iohk.ethereum.blockchain.sync._
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolver.{BranchResolvedSuccessful, StartBranchResolver}
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

class FastSyncBranchResolverSpec
    extends TestKit(ActorSystem("FastSyncBranchResolver_testing"))
    with AnyFreeSpecLike
    with ScalaFutures
    with NormalPatience
    with WithActorSystemShutDown { self =>
  implicit val timeout: Timeout = Timeout(30.seconds)

  import FastSyncBranchResolverSpec._

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

        val blocksSentFromPee: Map[Int, List[Block]] = Map(1 -> firstBatchBlockHeaders)

        saveBlocks(blocksSaved)
        val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSentFromPee)
        val fastSyncBranchResolver = creatFastSyncBranchResolver(sender.ref, etcPeerManager)

        val (peer, peerInfo) = getBestPeer
        (for {
          _ <- Task(sender.send(fastSyncBranchResolver, StartBranchResolver))
          _ <- fetchedHeaders.lastL.map(m => assert(m.last.number === 7))
          response <- Task(sender.expectMsg(BranchResolvedSuccessful(5, peer)))
          _ <- Task(stopController(fastSyncBranchResolver))
        } yield response).runSyncUnsafe()
      }

      "The chain is repaired doing binary searching with the new master peer and then remove the last invalid blocks" in new TestSetup {
        override implicit lazy val system = self.system
        override implicit val scheduler = Scheduler(system.dispatcher)

        val sender = TestProbe("sender")

        val commonBlocks: List[Block] = BlockHelpers.generateChain(
          5,
          BlockHelpers.genesis,
          block => block
        )

        val blocksSaved: List[Block] = commonBlocks :++ BlockHelpers.generateChain(
          5,
          commonBlocks.last,
          block => block
        )

        val blocksSavedInPeer: List[Block] = commonBlocks :++ BlockHelpers.generateChain(
          6,
          commonBlocks.last,
          block => block
        )

        val firstBatchBlockHeaders =
          blocksSavedInPeer.slice(blocksSavedInPeer.size - syncConfig.blockHeadersPerRequest, blocksSavedInPeer.size)

        val blocksSentFromPee: Map[Int, List[Block]] = Map(
          1 -> firstBatchBlockHeaders,
          2 -> List(blocksSavedInPeer.get(5).get),
          3 -> List(blocksSavedInPeer.get(8).get),
          4 -> List(blocksSavedInPeer.get(7).get),
          5 -> List(blocksSavedInPeer.get(6).get)
        )
        //blocks: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
        //the binary search asks the master peer for blocks in the following order: 6, 9, 8, 7

        saveBlocks(blocksSaved)
        val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSentFromPee)
        val fastSyncBranchResolver = creatFastSyncBranchResolver(sender.ref, etcPeerManager)

        val (peer, peerInfo) = getBestPeer
        (for {
          _ <- Task(sender.send(fastSyncBranchResolver, StartBranchResolver))
          _ <- fetchedHeaders.lastL.map(m => assert(m.last.number === 7))
          response <- Task(sender.expectMsg(BranchResolvedSuccessful(5, peer)))
          _ <- Task(stopController(fastSyncBranchResolver))
        } yield response).runSyncUnsafe()
      }
    }
  }

  trait TestSetup extends EphemBlockchainTestSetup with TestSyncConfig with TestSyncPeers {

    def peerId(number: Int): PeerId = PeerId(s"peer_$number")
    def getPeer(id: PeerId): Peer =
      Peer(new InetSocketAddress("127.0.0.1", 0), TestProbe(id.value).ref, incomingConnection = false)
    def getPeerInfo(peer: Peer, protocolVersion: Int = ProtocolVersions.PV64): PeerInfo = {
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

    val handshakedPeers: PeersMap = (0 to 5).toList.map((peerId _).andThen(getPeer)).fproduct(getPeerInfo(_)).toMap

    def saveBlocks(blocks: List[Block]): Unit = {
      blocks.foreach(block => blockchain.save(block, Nil, ChainWeight.totalDifficultyOnly(1), saveAsBestBlock = true))
    }

    def createEtcPeerManager(peers: PeersMap, blocks: Map[Int, List[Block]])(implicit
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

    def creatFastSyncBranchResolver(fastSync: ActorRef, etcPeerManager: ActorRef): ActorRef = system.actorOf(
      FastSyncBranchResolver.prop(
        fastSync = fastSync,
        peerEventBus = TestProbe("peer_event_bus").ref,
        etcPeerManager = etcPeerManager,
        blockchain = blockchain,
        syncConfig = syncConfig,
        appStateStorage = storagesInstance.storages.appStateStorage,
        scheduler = system.scheduler
      )
    )

    def stopController(actorRef: ActorRef): Unit = {
      awaitCond(gracefulStop(actorRef, actorAskTimeout.duration).futureValue)
    }

    def getBestPeer: (Peer, PeerInfo) = {
      handshakedPeers.toList.sortBy { case (_, peerInfo) => peerInfo.maxBlockNumber }(Ordering[BigInt].reverse).head
    }
  }
}

object FastSyncBranchResolverSpec extends Logger {

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
      peers: PeersMap,
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
