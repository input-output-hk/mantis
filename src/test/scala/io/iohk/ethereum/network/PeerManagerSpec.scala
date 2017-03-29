package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.util.Timeout
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import akka.actor._
import akka.agent.Agent
import akka.testkit.{TestActorRef, TestProbe}
import com.miguno.akka.testing.VirtualTime
import io.iohk.ethereum.crypto
import io.iohk.ethereum.utils.{Config, ServerStatus, NodeStatus}
import org.scalatest.{FlatSpec, Matchers}
import PeerActor.Status

class PeerManagerSpec extends FlatSpec with Matchers {

  implicit val timeout = Timeout(2.seconds)

  "PeerManager" should "try to connect to bootstrap nodes on startup" in new TestSetup {
    val peerManager = TestActorRef(Props(new PeerManagerActor(
      peerConfiguration, peerFactory, Some(time.scheduler))))(system)

    time.advance(800)
    Thread.sleep(200)

    val sender = TestProbe()

    createdPeers.head.expectMsgClass(classOf[PeerActor.ConnectTo])
    createdPeers(1).expectMsgClass(classOf[PeerActor.ConnectTo])

    peerManager.tell(PeerManagerActor.GetPeers, sender.ref)
    respondWithStatus(createdPeers.head, Status.Handshaking(0))
    respondWithStatus(createdPeers(1), Status.Handshaking(0))

    sender.expectMsgPF(2.seconds) {
      case peers: Peers =>
        peers.peers.size shouldBe 2
    }
  }

  it should "retry connections to remaining bootstrap nodes" in new TestSetup {
    val peerManager = TestActorRef(Props(new PeerManagerActor(
      peerConfiguration, peerFactory, Some(time.scheduler))))(system)

    time.advance(800)

    val sender = TestProbe()

    peerManager.tell(PeerManagerActor.GetPeers, sender.ref)

    createdPeers.head.expectMsgClass(classOf[PeerActor.ConnectTo])
    respondWithStatus(createdPeers.head, Status.Handshaking(0))

    createdPeers(1).expectMsgClass(classOf[PeerActor.ConnectTo])
    respondWithStatus(createdPeers(1), Status.Handshaking(0))

    sender.expectMsgPF(3.seconds) {
      case peers: Peers =>
        peers.peers.size shouldBe 2
    }

    createdPeers.head.ref ! PoisonPill

    peerManager.tell(PeerManagerActor.GetPeers, sender.ref)

    respondWithStatus(createdPeers(1), Status.Handshaking(0))

    sender.expectMsgPF(2.seconds) {
      case peers: Peers =>
        peers.peers.size shouldBe 1
    }

    time.advance(1000) // wait for next scan

    respondWithStatus(createdPeers(1), Status.Handshaking(0))

    Thread.sleep(200)

    createdPeers(2).expectMsgClass(classOf[PeerActor.ConnectTo])

    peerManager.tell(PeerManagerActor.GetPeers, sender.ref)
    respondWithStatus(createdPeers(1), Status.Handshaking(0))
    respondWithStatus(createdPeers(2), Status.Handshaking(0))

    sender.expectMsgPF(2.seconds) {
      case peers: Peers =>
        peers.peers.size shouldBe 2
    }
  }

  it should "disconnect the peer when limit is reached" in new TestSetup {
    val peerManager = TestActorRef(Props(new PeerManagerActor(
      peerConfiguration, peerFactory, Some(time.scheduler))))

    time.advance(800) // connect to 2 bootstrap peers
    Thread.sleep(200)

    createdPeers.size shouldBe 2
    createdPeers.head.expectMsgClass(classOf[PeerActor.ConnectTo])
    createdPeers(1).expectMsgClass(classOf[PeerActor.ConnectTo])

    Thread.sleep(200)

    peerManager ! PeerManagerActor.HandlePeerConnection(system.deadLetters, new InetSocketAddress(9000))

    respondWithStatus(createdPeers.head, Status.Handshaking(0))
    respondWithStatus(createdPeers(1), Status.Handshaking(0))

    Thread.sleep(400)

    createdPeers.size shouldBe 3
    createdPeers.last.expectMsgClass(classOf[PeerActor.HandleConnection])

    Thread.sleep(400)

    peerManager ! PeerManagerActor.HandlePeerConnection(system.deadLetters, new InetSocketAddress(1000))

    respondWithStatus(createdPeers.head, Status.Handshaking(0))
    respondWithStatus(createdPeers(1), Status.Handshaking(0))
    respondWithStatus(createdPeers(2), Status.Handshaking(0))

    Thread.sleep(400)

    createdPeers.size shouldBe 4
    createdPeers.last.expectMsgClass(classOf[PeerActor.HandleConnection])
    createdPeers.last.expectMsg(PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers))
  }

  trait TestSetup {
    implicit val system = ActorSystem("PeerManagerActorSpec_System")

    val time = new VirtualTime

    val nodeKey = crypto.generateKeyPair()

    val nodeStatus = NodeStatus(
      key = nodeKey,
      serverStatus = ServerStatus.NotListening)

    val nodeStatusHolder = Agent(nodeStatus)

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
