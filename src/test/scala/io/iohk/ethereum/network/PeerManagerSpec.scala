package io.iohk.ethereum.network

import java.net.InetSocketAddress

import io.iohk.ethereum.network.PeerActor.Status.Handshaking
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Seconds, Milliseconds, Span}
import akka.actor._
import akka.testkit.{TestActorRef, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.utils.Config
import org.scalatest.{FlatSpec, Matchers}
import PeerActor.Status

class PeerManagerSpec extends FlatSpec with Matchers with Eventually {

  override implicit val patienceConfig = PatienceConfig(Span(5, Seconds), Span(100, Milliseconds))

  "PeerManager" should "try to connect to bootstrap nodes on startup" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(
      peerConfiguration, peerFactory, Some(time.scheduler))))(system)

    time.advance(800) // wait for bootstrap nodes scan

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 1
    }

    peerManager ! "Thank you Akka for great testing framework! (yes, this message is actually needed to trigger unstashAll() in TestActorRef)"

    createdPeers.head.expectMsgClass(classOf[PeerActor.ConnectTo])
    respondWithStatus(createdPeers.head, Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }
  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    val peerManager = TestActorRef[PeerManagerActor](Props(new PeerManagerActor(
      peerConfiguration, peerFactory, Some(time.scheduler))))(system)

    time.advance(800)

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 1
    }

    createdPeers.head.expectMsgClass(classOf[PeerActor.ConnectTo])

    peerManager ! "trigger stashed messages..."
    respondWithStatus(createdPeers.head, Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }

    createdPeers(1).expectMsgClass(classOf[PeerActor.ConnectTo])

    createdPeers.head.ref ! PoisonPill

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
      peerConfiguration, peerFactory, Some(time.scheduler))))

    time.advance(800) // connect to 2 bootstrap peers

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 2
    }

    peerManager ! PeerManagerActor.HandlePeerConnection(system.deadLetters, new InetSocketAddress(9000))

    createdPeers.head.expectMsgClass(classOf[PeerActor.ConnectTo])
    createdPeers(1).expectMsgClass(classOf[PeerActor.ConnectTo])

    respondWithStatus(createdPeers.head, Status.Handshaking(0))
    respondWithStatus(createdPeers(1), Status.Handshaking(0))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 3
    }

    createdPeers(2).expectMsgClass(classOf[PeerActor.HandleConnection])

    peerManager ! PeerManagerActor.HandlePeerConnection(system.deadLetters, new InetSocketAddress(1000))

    respondWithStatus(createdPeers.head, Status.Handshaking(0))
    respondWithStatus(createdPeers(1), Status.Handshaking(0))
    respondWithStatus(createdPeers(2), Status.Handshaking(1))

    eventually {
      peerManager.underlyingActor.peers.size shouldBe 4
    }

    createdPeers(2).expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))
    createdPeers(3).expectMsgClass(classOf[PeerActor.HandleConnection])
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerManagerActorSpec_System")

    val time = new VirtualTime

    var createdPeers: Seq[TestProbe] = Nil

    val peerConfiguration = Config.Network.peer

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
