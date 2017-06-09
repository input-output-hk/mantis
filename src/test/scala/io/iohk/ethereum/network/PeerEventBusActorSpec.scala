package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, MessageToPeer, PeerDisconnected, PeerHandshakeSuccessful}
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.PeersInfoHolderActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

class PeerEventBusActorSpec extends FlatSpec with Matchers {

  "PeerEventBusActor" should "relay messages received to subscribers" in new TestSetup {

    val probe1 = TestProbe()(system)
    val probe2 = TestProbe()(system)
    val classifier1 = MessageReceivedClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    val classifier2 = MessageReceivedClassifier(Set(Ping.code), PeerSelector.AllPeers)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(classifier1), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(classifier2), probe2.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
    probe2.expectMsg(msgFromPeer)

    peerMessageBusActor.tell(PeerEventBusActor.Unsubscribe(classifier1), probe1.ref)

    val msgFromPeer2 = MessageFromPeer(Ping(), PeerId("99"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
    probe2.expectMsg(msgFromPeer2)
  }

  it should "relay messages sent to subscribers" in new TestSetup {

    val probe1 = TestProbe()(system)
    val probe2 = TestProbe()(system)
    val classifier1 = MessageSentClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    val classifier2 = MessageSentClassifier(Set(Ping.code), PeerSelector.AllPeers)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(classifier1), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(classifier2), probe2.ref)

    val msgToPeer = MessageToPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgToPeer)

    probe1.expectMsg(msgToPeer)
    probe2.expectMsg(msgToPeer)

    peerMessageBusActor.tell(PeerEventBusActor.Unsubscribe(classifier1), probe1.ref)

    val msgFromPeer2 = MessageToPeer(Ping(), PeerId("99"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
    probe2.expectMsg(msgFromPeer2)
  }

  it should "only relay matching message codes" in new TestSetup {

    val probe1 = TestProbe()
    val classifier1 = MessageReceivedClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(classifier1), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)

    val msgFromPeer2 = MessageFromPeer(Pong(), PeerId("1"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
  }

  it should "relay peers disconnecting to its subscribers" in new TestSetup {

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))), probe2.ref)

    val msgPeerDisconnected = PeerDisconnected(PeerId("2"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)

    probe1.expectMsg(msgPeerDisconnected)
    probe2.expectMsg(msgPeerDisconnected)

    peerMessageBusActor.tell(PeerEventBusActor.Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))), probe1.ref)

    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)
    probe1.expectNoMsg()
    probe2.expectMsg(msgPeerDisconnected)
  }

  it should "relay peers handshaked to its subscribers" in new TestSetup {

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe2.ref)

    val peerHandshaked = new Peer(new InetSocketAddress("127.0.0.1", 0), TestProbe().ref)
    val msgPeerHandshaked = PeerHandshakeSuccessful(peerHandshaked, initialPeerInfo)
    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)

    probe1.expectMsg(msgPeerHandshaked)
    probe2.expectMsg(msgPeerHandshaked)

    peerMessageBusActor.tell(PeerEventBusActor.Unsubscribe(PeerHandshaked), probe1.ref)

    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)
    probe1.expectNoMsg()
    probe2.expectMsg(msgPeerHandshaked)
  }

  trait TestSetup {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerEventBusActor.props)

    val peerStatus = Status(
      protocolVersion = Versions.PV63,
      networkId = 1,
      totalDifficulty = BigInt(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      totalDifficulty = peerStatus.totalDifficulty,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number
    )

  }

}
