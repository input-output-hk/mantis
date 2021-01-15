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
import io.iohk.ethereum.domain.{Block, ChainWeight}
import io.iohk.ethereum.network.EtcPeerManagerActor._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.ProtocolVersions
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.{BlockHelpers, NormalPatience, WithActorSystemShutDown}
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpecLike

import scala.concurrent.duration.DurationInt
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
        val sender = TestProbe("sender")(system)

        val commonBlocks: List[Block] = BlockHelpers.generateChain(
          5,
          BlockHelpers.genesis,
          block => block
        )
        val blocksSaved: List[Block] = commonBlocks :++ BlockHelpers.generateChain(
          2,
          commonBlocks.last,
          block => block
        )

        val blocksSavedInPeer: List[Block] = commonBlocks :++ BlockHelpers.generateChain(
          3,
          commonBlocks.last,
          block => block
        )

        saveBlocks(blocksSaved)
        val etcPeerManager = createEtcPeerManager(handshakedPeers, blocksSavedInPeer)(scheduler)
        val fastSyncBranchResolver = creatFastSyncBranchResolver(sender.ref, etcPeerManager)
        sender.send(fastSyncBranchResolver, StartBranchResolver)

        sender.expectMsg(BranchResolvedSuccessful)
        assert(blockchain.getBestBlockNumber() === 5)
        stopController(fastSyncBranchResolver)
      }

      //TODO: work in progress
      /*"The chain is repaired doing binary searching with the new master peer and then remove the last invalid blocks" in new TestSetup {

      }*/
    }
  }

  trait TestSetup extends EphemBlockchainTestSetup with TestSyncConfig with TestSyncPeers {
    override implicit lazy val system = self.system
    override implicit val scheduler = Scheduler(system.dispatcher)

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

    def createEtcPeerManager(peers: PeersMap, blocks: List[Block])(implicit scheduler: Scheduler): ActorRef = {
      val etcPeerManager = TestProbe("etc_peer_manager")
      val autoPilot =
        new EtcPeerManagerAutoPilot(
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
  }
}

object FastSyncBranchResolverSpec {

  private val peersConnectedDeferred = Deferred.unsafe[Task, Unit]

  class EtcPeerManagerAutoPilot(
      peersConnected: Deferred[Task, Unit],
      peers: PeersMap,
      blocks: List[Block]
  )(implicit scheduler: Scheduler)
      extends AutoPilot {
    def run(sender: ActorRef, msg: Any): EtcPeerManagerAutoPilot = {
      msg match {
        case EtcPeerManagerActor.GetHandshakedPeers =>
          sender ! EtcPeerManagerActor.HandshakedPeers(peers)
          peersConnected.complete(()).onErrorHandle(_ => ()).runSyncUnsafe()
        case sendMsg @ EtcPeerManagerActor.SendMessage(rawMsg, peerId) =>
          val response = rawMsg.underlyingMsg match {
            case GetBlockHeaders(startingBlock, maxHeaders, _, false) =>
              BlockHeaders(blocks.map(_.header))
          }
          val theResponse = MessageFromPeer(response, peerId)
          sender ! theResponse
      }
      this
    }
  }
}
