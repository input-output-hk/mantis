package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor._
import akka.testkit.{TestActorRef, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.network.PeerActor.IncomingConnectionHandshakeSuccess
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, PeerDiscoveryManager}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Config
import org.scalatest.concurrent.Eventually
import org.scalatest.{FlatSpec, Matchers}

class PeerManagerSpec extends FlatSpec with Matchers with Eventually with NormalPatience {

  "PeerManager" should "try to connect to bootstrap nodes on startup" in new TestSetup {

    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))(system)
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // wait for bootstrap nodes scan

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.managerState.peers.size shouldBe 2
    }
  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))(system)
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000)

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.managerState.peers.size shouldBe 2
    }

    createdPeers(1).expectMsgClass(classOf[PeerActor.ConnectTo])

    createdPeers.head.ref ! PoisonPill

    eventually {
      peerManager.underlyingActor.managerState.peers.size shouldBe 1
    }

    time.advance(21000) // wait for next scan

    peerManager ! "trigger stashed messages..."

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.managerState.peers.size shouldBe 2
    }
  }

  it should "publish disconnect messages from peers" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // connect to 2 bootstrap peers

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.managerState.peers.size shouldBe 2
    }

    createdPeers.head.ref ! PoisonPill

    time.advance(21000) // connect to 2 bootstrap peers

    peerEventBus.expectMsg(Publish(PeerDisconnected(PeerId(createdPeers.head.ref.path.name))))
  }

  it should "not handle the connection from a peer that's already connected" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))(system)
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // connect to 2 bootstrap peers

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.managerState.peers.size shouldBe 2
    }

    val connection = TestProbe()

    val watcher = TestProbe()
    watcher.watch(connection.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(connection.ref, new InetSocketAddress("127.0.0.1", 30340))

    watcher.expectMsgClass(classOf[Terminated])
  }

  it should "handle pending and handshaked incoming peers" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))(system)
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // connect to 2 bootstrap peers

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.managerState.peers.size shouldBe 2
    }

    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection1.ref, incomingPeerAddress1)

    eventually {
      peerManager.underlyingActor.managerState.pendingIncomingPeers.size shouldBe 1
    }

    val peer = Peer(incomingPeerAddress1, createdPeers(2).ref, true)

    createdPeers(2).expectMsg(PeerActor.HandleConnection(incomingConnection1.ref, incomingPeerAddress1))
    createdPeers(2).reply(IncomingConnectionHandshakeSuccess(peer.id, peer))

    eventually {
      peerManager.underlyingActor.managerState.pendingIncomingPeers.size shouldBe 0
      peerManager.underlyingActor.managerState.peers.size shouldBe 3
    }

    val watcher = TestProbe()
    watcher.watch(incomingConnection3.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection2.ref, incomingPeerAddress2)
    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection3.ref, incomingPeerAddress3)

    eventually {
      peerManager.underlyingActor.managerState.pendingIncomingPeers.size shouldBe 1
    }

    watcher.expectMsgClass(classOf[Terminated])

    val peer2 = Peer(incomingPeerAddress2, createdPeers(3).ref, true)

    createdPeers(3).expectMsg(PeerActor.HandleConnection(incomingConnection2.ref, incomingPeerAddress2))
    createdPeers(3).reply(IncomingConnectionHandshakeSuccess(peer2.id, peer2))
    createdPeers(3).expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))

    eventually {
      peerManager.underlyingActor.managerState.pendingIncomingPeers.size shouldBe 0
      peerManager.underlyingActor.managerState.peers.size shouldBe 3
    }
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerManagerActorSpec_System")

    val time = new VirtualTime

    var createdPeers: Seq[TestProbe] = Nil

    val peerConfiguration = Config.Network.peer

    val peerDiscoveryManager = TestProbe()
    val bootstrapNodes = DiscoveryConfig(Config.config).bootstrapNodes.map(PeerDiscoveryManager.DiscoveryNodeInfo.fromNode)

    val peerEventBus = TestProbe()

    val knownNodesManager = TestProbe()

    val peerFactory: (ActorContext, InetSocketAddress, Boolean) => ActorRef = { (ctx, addr, _) =>
      val peer = TestProbe()
      createdPeers :+= peer
      peer.ref
    }

    def respondWithStatus(peer: TestProbe, status: PeerActor.Status): Unit = {
      peer.expectMsg(PeerActor.GetStatus)
      peer.reply(PeerActor.StatusResponse(status))
    }


    val incomingConnection1 = TestProbe()
    val incomingConnection2 = TestProbe()
    val incomingConnection3 = TestProbe()
    val incomingPeerAddress1 = new InetSocketAddress("127.0.0.2", 30340)
    val incomingPeerAddress2 = new InetSocketAddress("127.0.0.3", 30340)
    val incomingPeerAddress3 = new InetSocketAddress("127.0.0.4", 30340)

  }

}
