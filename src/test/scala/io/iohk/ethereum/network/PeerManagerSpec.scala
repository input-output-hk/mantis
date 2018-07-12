package io.iohk.ethereum.network

import java.net.{ InetSocketAddress, URI }

import akka.actor._
import akka.testkit.{ TestActorRef, TestProbe }
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerActor.{ IncomingConnectionHandshakeSuccess, PeerClosedConnection }
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.discovery.{ DiscoveryConfig, PeerDiscoveryManager }
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.{ Fixtures, NormalPatience }
import org.scalatest.concurrent.Eventually
import org.scalatest.{ FlatSpec, Matchers }

// scalastyle:off magic.number
class PeerManagerSpec extends FlatSpec with Matchers with Eventually with NormalPatience {

  "PeerManager" should "try to connect to bootstrap nodes on startup" in new TestSetup {

    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // wait for bootstrap nodes scan

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))
    knownNodesManager.expectMsg(KnownNodesManager.GetKnownNodes)
    knownNodesManager.reply(KnownNodesManager.KnownNodes(knownNodes))
  }

  it should "blacklist peer that fail to establish tcp connection" in new TestSetup {

    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // wait for bootstrap nodes scan

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))
    knownNodesManager.expectMsg(KnownNodesManager.GetKnownNodes)
    knownNodesManager.reply(KnownNodesManager.KnownNodes(knownNodes))

    peerManager ! PeerClosedConnection(peer1.remoteAddress.getHostString, Disconnect.Reasons.Other)

  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000)

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    peer1Probe.ref ! PeerManagerActor.ConnectToPeer(new URI("encode://localhost:9000"))
    peer1Probe.expectMsgClass(classOf[PeerManagerActor.ConnectToPeer])

    peer1Probe.ref ! PoisonPill
    peer1Probe.unwatch(peer1Probe.ref)

    time.advance(21000) // wait for next scan

    peerManager ! "trigger stashed messages..."

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

  }

  it should "publish disconnect messages from peers" in new TestSetup {
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // connect to 2 bootstrap peers

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    peer1Probe.ref ! PoisonPill
    peer1Probe.unwatch(peer1Probe.ref)

    time.advance(21000) // connect to 2 bootstrap peers

    peerEventBus.expectMsg(Publish(PeerDisconnected(peer1.id)))
  }

  it should "not handle the connection from a peer that's already connected" in new TestSetup {
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // connect to 2 bootstrap peers

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    val watcher = TestProbe()
    watcher.watch(peer1Probe.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(peer1Probe.ref, peer1.remoteAddress)

    watcher.expectMsgClass(classOf[Terminated])
  }

  it should "handle pending and handshaked incoming peers" in new TestSetup {
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // connect to 2 bootstrap peers

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodesInfo)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodesInfo(bootstrapNodes))

    peerManager ! PeerManagerActor.HandlePeerConnection(peer2Probe.ref, peer2.remoteAddress)

    peer2Probe.expectMsg(PeerActor.HandleConnection(peer2Probe.ref, peer2.remoteAddress))
    peer2Probe.reply(IncomingConnectionHandshakeSuccess(peer2))

    val watcher = TestProbe()
    watcher.watch(peer3Probe.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(peer1Probe.ref, peer1.remoteAddress)
    peerManager ! PeerManagerActor.HandlePeerConnection(peer3Probe.ref, peer3.remoteAddress)

    watcher.expectMsgClass(classOf[Terminated])

    peer3Probe.expectMsg(PeerActor.HandleConnection(peer1Probe.ref, peer1.remoteAddress))
    peer3Probe.reply(IncomingConnectionHandshakeSuccess(peer1))
    peer3Probe.expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))

    // Peer(3) after receiving disconnect schedules poison pill for himself
    peer3Probe.ref ! PoisonPill

    peerEventBus.expectMsg(Publish(PeerDisconnected(peer3.id)))

  }

  trait TestSetup {
    implicit lazy val system: ActorSystem = ActorSystem("PeerManagerActorSpec_System")

    val time = new VirtualTime

    val peerConfiguration: PeerConfiguration = Config.Network.peer

    val peerDiscoveryManager = TestProbe()
    val peerEventBus = TestProbe()
    val knownNodesManager = TestProbe()

    val bootstrapNodes: Set[PeerDiscoveryManager.DiscoveryNodeInfo] =
      DiscoveryConfig(Config.config).bootstrapNodes.map(PeerDiscoveryManager.DiscoveryNodeInfo.fromNode)
    val knownNodes: Set[URI] = Set(URI.create("https://example.com"))

    val peerFactory: (ActorContext, InetSocketAddress, Boolean) => ActorRef = { (ctx, addr, _) =>
      val peer = TestProbe()
      peer.ref
    }

    val peerStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 1,
      totalDifficulty = BigInt("10000"),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      totalDifficulty = peerStatus.totalDifficulty,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number
    )

    val peer1Probe = TestProbe()
    val peer1 = Peer(new InetSocketAddress("127.0.0.1", 30340), peer1Probe.ref, incomingConnection = false)
    val peer1Info: PeerInfo = initialPeerInfo.withForkAccepted(false)
    val peer2Probe = TestProbe()
    val peer2 = Peer(new InetSocketAddress("127.0.0.1", 30341), peer2Probe.ref, incomingConnection = true)
    val peer2Info: PeerInfo = initialPeerInfo.withForkAccepted(false)
    val peer3Probe = TestProbe()
    val peer3 = Peer(new InetSocketAddress("127.0.0.1", 30342), peer3Probe.ref, incomingConnection = false)

    val peerManager = TestActorRef(Props(new PeerManagerActor(
      peerEventBus.ref, peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler)
    )))

  }

}
