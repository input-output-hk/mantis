package io.iohk.ethereum.network

import java.net.{ InetSocketAddress, URI }

import akka.actor._
import akka.testkit.{ TestActorRef, TestProbe }
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.network.PeerActor.{ IncomingConnectionHandshakeSuccess, PeerClosedConnection }
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.discovery.{ DiscoveryConfig, PeerDiscoveryManager }
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Config
import org.scalatest.concurrent.Eventually
import org.scalatest.{ FlatSpec, Matchers }

// scalastyle:off magic.number
class PeerManagerSpec extends FlatSpec with Matchers with Eventually with NormalPatience {

  "PeerManager" should "try to connect to bootstrap and known nodes on startup" in new TestSetup {
    startConnecting()

    system.terminate()
  }

  it should "blacklist peer that fail to establish tcp connection" in new TestSetup {

    startConnecting()

    val probe: TestProbe = createdPeers.head

    probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection1.ref, incomingPeerAddress1)

    val probe2: TestProbe = createdPeers(2)
    val peer = Peer(incomingPeerAddress1, probe2.ref, incomingConnection = true)

    peerManager ! PeerClosedConnection(peer.remoteAddress.getHostString, Disconnect.Reasons.Other)

    eventually{
      peerManager.underlyingActor.blacklistedPeers.size shouldEqual 1
    }

    system.terminate()

  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    startConnecting()

    val probe: TestProbe = createdPeers.head

    probe.expectMsgClass(classOf[PeerActor.ConnectTo])

    probe.ref ! PoisonPill

    time.advance(21000) // wait for next scan

    peerManager ! "trigger stashed messages..."

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))
    system.terminate()

  }

  it should "publish disconnect messages from peers" in new TestSetup {
    startConnecting()

    val probe: TestProbe = createdPeers.head

    probe.ref ! PoisonPill

    time.advance(21000) // connect to 2 bootstrap peers

    peerEventBus.expectMsg(Publish(PeerDisconnected(PeerId(probe.ref.path.name))))
    system.terminate()
  }

  it should "not handle the connection from a peer that's already connected" in new TestSetup {
    startConnecting()

    val connection = TestProbe()

    val watcher = TestProbe()
    watcher.watch(connection.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(connection.ref, new InetSocketAddress("127.0.0.1", 30340))

    watcher.expectMsgClass(classOf[Terminated])
    system.terminate()

  }

  it should "handle pending and handshaked incoming peers" in new TestSetup {
    startConnecting()

    val probe1: TestProbe = createdPeers.head

    probe1.expectMsgClass(classOf[PeerActor.ConnectTo])

    time.advance(21000) // wait for next scan

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection1.ref, incomingPeerAddress1)

    val probe2: TestProbe = createdPeers(2)
    val peer = Peer(incomingPeerAddress1, probe2.ref, incomingConnection = true)

    probe2.expectMsg(PeerActor.HandleConnection(incomingConnection1.ref, incomingPeerAddress1))
    probe2.reply(IncomingConnectionHandshakeSuccess(peer))

    val watcher = TestProbe()
    watcher.watch(incomingConnection3.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection2.ref, incomingPeerAddress2)
    peerManager ! PeerManagerActor.HandlePeerConnection(incomingConnection3.ref, incomingPeerAddress3)

    watcher.expectMsgClass(classOf[Terminated])

    val probe3: TestProbe = createdPeers(3)

    val secondPeer = Peer(incomingPeerAddress2, probe3.ref, incomingConnection = true)

    probe3.expectMsg(PeerActor.HandleConnection(incomingConnection2.ref, incomingPeerAddress2))
    probe3.reply(IncomingConnectionHandshakeSuccess(secondPeer))
    probe3.expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))

    // Peer(3) after receiving disconnect schedules poison pill for himself
    probe3.ref ! PoisonPill

    peerEventBus.expectMsg(Publish(PeerDisconnected(PeerId(probe3.ref.path.name))))
    system.terminate()
  }

  trait TestSetup {
    implicit lazy val system: ActorSystem = ActorSystem("PeerManagerActorSpec_System")

    val time = new VirtualTime

    var createdPeers: Seq[TestProbe] = Seq.empty

    val peerConfiguration: PeerConfiguration = Config.Network.peer
    val peerDiscoveryManager = TestProbe()
    val peerEventBus = TestProbe()
    val knownNodesManager = TestProbe()

    val bootstrapNodes: Set[PeerDiscoveryManager.DiscoveryNodeInfo] =
      DiscoveryConfig(Config.config).bootstrapNodes.map(PeerDiscoveryManager.DiscoveryNodeInfo.fromNode)
    val knownNodes: Set[URI] = Set.empty

    val peerFactory: (ActorContext, InetSocketAddress, Boolean) => ActorRef = { (ctx, addr, _) =>
      val peer = TestProbe()
      createdPeers :+= peer
      peer.ref
    }

    val incomingConnection1 = TestProbe()
    val incomingConnection2 = TestProbe()
    val incomingConnection3 = TestProbe()
    val port = 30340
    val incomingPeerAddress1 = new InetSocketAddress("127.0.0.2", port)
    val incomingPeerAddress2 = new InetSocketAddress("127.0.0.3", port)
    val incomingPeerAddress3 = new InetSocketAddress("127.0.0.4", port)

    val peerManager: TestActorRef[PeerManagerActor] = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))(system)

    def startConnecting(): Unit = {
      peerManager ! PeerManagerActor.StartConnecting

      time.advance(6000) // wait for bootstrap nodes scan

      peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
      peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))
      knownNodesManager.expectMsg(KnownNodesManager.GetKnownNodes)
      knownNodesManager.reply(KnownNodesManager.KnownNodes(knownNodes))
    }
  }

}
