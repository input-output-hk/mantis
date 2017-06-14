package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected, PeerHandshakeSuccessful}
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

class PeerEventBusActorSpec extends FlatSpec with Matchers {

  "PeerEventBusActor" should "relay messages received to subscribers" in new TestSetup {

    val probe1 = TestProbe()(system)
    val probe2 = TestProbe()(system)
    val classifier1 = MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    val classifier2 = MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(classifier1), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(classifier2), probe2.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
    probe2.expectMsg(msgFromPeer)

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(classifier1), probe1.ref)

    val msgFromPeer2 = MessageFromPeer(Ping(), PeerId("99"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
    probe2.expectMsg(msgFromPeer2)
  }

  it should "only relay matching message codes" in new TestSetup {

    val probe1 = TestProbe()
    val classifier1 = MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(classifier1), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)

    val msgFromPeer2 = MessageFromPeer(Pong(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
  }

  it should "relay peers disconnecting to its subscribers" in new TestSetup {

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))), probe2.ref)

    val msgPeerDisconnected = PeerDisconnected(PeerId("2"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)

    probe1.expectMsg(msgPeerDisconnected)
    probe2.expectMsg(msgPeerDisconnected)

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))), probe1.ref)

    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)
    probe1.expectNoMsg()
    probe2.expectMsg(msgPeerDisconnected)
  }

  it should "relay peers handshaked to its subscribers" in new TestSetup {

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe2.ref)

    val peerHandshaked = new Peer(new InetSocketAddress("127.0.0.1", 0), TestProbe().ref)
    val msgPeerHandshaked = PeerHandshakeSuccessful(peerHandshaked, initialPeerInfo)
    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)

    probe1.expectMsg(msgPeerHandshaked)
    probe2.expectMsg(msgPeerHandshaked)

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(PeerHandshaked), probe1.ref)

    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)
    probe1.expectNoMsg()
    probe2.expectMsg(msgPeerHandshaked)
  }

  it should "relay a single notification when subscribed twice to the same message code" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
    probe1.expectNoMsg()
  }

  it should "allow to handle subscriptions using AllPeers and WithId PeerSelector at the same time" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    // Receive a single notification
    probe1.expectMsg(msgFromPeer)
    probe1.expectNoMsg()

    val msgFromPeer2 = MessageFromPeer(Ping(), PeerId("2"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    // Receive based on AllPeers subscription
    probe1.expectMsg(msgFromPeer2)

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)), probe1.ref)
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    // Still received after unsubscribing from AllPeers
    probe1.expectMsg(msgFromPeer)
  }

  it should "allow to subscribe to new messages" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer = MessageFromPeer(Pong(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
  }

  it should "not change subscriptions when subscribing to empty set" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
  }

  it should "allow to unsubscribe from messages" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer1 = MessageFromPeer(Ping(), PeerId("1"))
    val msgFromPeer2 = MessageFromPeer(Pong(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer1)
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    probe1.expectMsg(msgFromPeer1)
    probe1.expectMsg(msgFromPeer2)

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(MessageClassifier(Set(Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer1)
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    probe1.expectMsg(msgFromPeer1)
    probe1.expectNoMsg()

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(), probe1.ref)

    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer1)
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    probe1.expectNoMsg()
  }

  trait TestSetup {
    implicit val system = ActorSystem("test-system")

    val peerEventBusActor = system.actorOf(PeerEventBusActor.props)

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
