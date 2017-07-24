package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import io.iohk.ethereum.network.PeerActor.Status.Handshaking
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import org.scalatest.concurrent.Eventually
import akka.actor._
import akka.testkit.{TestActorRef, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.utils.Config
import org.scalatest.{FlatSpec, Matchers}
import PeerActor.Status
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, PeerDiscoveryManager}

class PeerManagerSpec extends FlatSpec with Matchers with Eventually with NormalPatience {

  "PeerManager" should "try to connect to bootstrap nodes on startup" in new TestSetup {

    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))(system)
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // wait for bootstrap nodes scan

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodes)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodes(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }
  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))(system)
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000)

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodes)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodes(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }

    createdPeers(1).expectMsgClass(classOf[PeerActor.ConnectTo])

    createdPeers.head.ref ! PoisonPill

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 1
    }

    time.advance(21000) // wait for next scan

    peerManager ! "trigger stashed messages..."

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodes)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodes(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }
  }

  it should "publish disconnect messages from peers" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(peerEventBus.ref,
      peerDiscoveryManager.ref, peerConfiguration, knownNodesManager.ref, peerFactory, Some(time.scheduler))))
    peerManager ! PeerManagerActor.StartConnecting

    time.advance(6000) // connect to 2 bootstrap peers

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodes)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodes(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
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

    peerDiscoveryManager.expectMsg(PeerDiscoveryManager.GetDiscoveredNodes)
    peerDiscoveryManager.reply(PeerDiscoveryManager.DiscoveredNodes(bootstrapNodes))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }

    val connection = TestProbe()

    val watcher = TestProbe()
    watcher.watch(connection.ref)

    peerManager ! PeerManagerActor.HandlePeerConnection(connection.ref, new InetSocketAddress("127.0.0.1", 30340))

    watcher.expectMsgClass(classOf[Terminated])
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerManagerActorSpec_System")

    val time = new VirtualTime

    var createdPeers: Seq[TestProbe] = Nil

    val peerConfiguration = Config.Network.peer

    val peerDiscoveryManager = TestProbe()
    val bootstrapNodes = DiscoveryConfig(Config.config).bootstrapNodes.map(s => PeerDiscoveryManager.Node.fromUri(new URI(s)))

    val peerEventBus = TestProbe()

    val knownNodesManager = TestProbe()

    val peerFactory: (ActorContext, InetSocketAddress) => ActorRef = { (ctx, addr) =>
      val peer = TestProbe()
      createdPeers :+= peer
      peer.ref
    }

    def respondWithStatus(peer: TestProbe, status: PeerActor.Status): Unit = {
      peer.expectMsg(PeerActor.GetStatus)
      peer.reply(PeerActor.StatusResponse(status))
    }

  }

}
