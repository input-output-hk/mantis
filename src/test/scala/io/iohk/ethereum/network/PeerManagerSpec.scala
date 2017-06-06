package io.iohk.ethereum.network

import java.net.InetSocketAddress

import io.iohk.ethereum.network.PeerActor.Status.Handshaking
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Milliseconds, Seconds, Span}
import akka.actor._
import akka.testkit.{TestActorRef, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.utils.Config
import org.scalatest.{FlatSpec, Matchers}
import PeerActor.{ConnectionRequest, Status}
import io.iohk.ethereum.network.PeerEventBusActor.{PeerEvent, Publish}

class PeerManagerSpec extends FlatSpec with Matchers with Eventually {

  override implicit val patienceConfig = PatienceConfig(Span(5, Seconds), Span(100, Milliseconds))

  "PeerManager" should "try to connect to bootstrap nodes on startup" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(
      peerConfiguration, peerEventBus.ref, peerFactory, Some(time.scheduler),
      Config.Network.Discovery.bootstrapNodes)))(system)

    time.advance(800) // wait for bootstrap nodes scan

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 1
    }

    peerManager ! "Thank you Akka for great testing framework! (yes, this message is actually needed to trigger unstashAll() in TestActorRef)"

    respondWithStatus(createdPeers.head, Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }
  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(
      peerConfiguration, peerEventBus.ref, peerFactory, Some(time.scheduler),
      Config.Network.Discovery.bootstrapNodes)))(system)

    time.advance(800)

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 1
    }

    peerManager ! "trigger stashed messages..."
    respondWithStatus(createdPeers.head, Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }

    createdPeers.head.ref ! PoisonPill
    peerEventBus.send(peerManager, PeerEvent.PeerDisconnected(createdPeersIds.head))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 1
    }

    time.advance(1000) // wait for next scan

    peerManager ! "trigger stashed messages..."
    respondWithStatus(createdPeers(1), Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }
  }

  it should "disconnect the worst handshaking peer when limit is reached" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(
      peerConfiguration, peerEventBus.ref, peerFactory, Some(time.scheduler), Config.Network.Discovery.bootstrapNodes)))

    time.advance(800) // connect to 2 bootstrap peers

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 1
    }

    peerManager ! "trigger stashed messages..."
    respondWithStatus(createdPeers.head, Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }

    peerManager ! PeerManagerActor.HandlePeerConnection(system.deadLetters, new InetSocketAddress(9000))

    peerManager ! "trigger stashed messages..."
    respondWithStatus(createdPeers.head, Status.Handshaking(0))
    respondWithStatus(createdPeers(1), Status.Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 3
    }

    peerManager ! PeerManagerActor.HandlePeerConnection(system.deadLetters, new InetSocketAddress(1000))

    peerManager ! "trigger stashed messages..."
    respondWithStatus(createdPeers.head, Status.Handshaking(0))
    respondWithStatus(createdPeers(1), Status.Handshaking(0))
    respondWithStatus(createdPeers(2), Status.Handshaking(1))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 4
    }

    createdPeers(2).expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerManagerActorSpec_System")

    val time = new VirtualTime

    var createdPeers: Seq[TestProbe] = Nil
    var createdPeersIds: Seq[PeerId] = Nil

    val peerConfiguration = Config.Network.peer

    val peerEventBus = TestProbe()

    val peerFactory: (ActorContext, InetSocketAddress, ConnectionRequest) => Peer = { (ctx, addr, req) =>
      val peer = TestProbe()
      createdPeers :+= peer
      val createdPeer = new PeerImpl(addr, peer.ref, peerEventBus.ref)
      createdPeersIds :+= createdPeer.id
      createdPeer
    }

    def respondWithStatus(peer: TestProbe, status: PeerActor.Status): Unit = {
      peer.expectMsg(PeerActor.GetStatus)
      peer.reply(PeerActor.StatusResponse(status))
    }

  }

}
