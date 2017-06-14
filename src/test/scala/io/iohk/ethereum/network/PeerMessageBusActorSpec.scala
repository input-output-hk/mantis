package io.iohk.ethereum.network

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.network.PeerMessageBusActor.{MessageClassifier, MessageFromPeer, PeerSelector}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

class PeerMessageBusActorSpec extends FlatSpec with Matchers {

  "PeerMessageBusActor" should "relay messages to subscribers" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerMessageBusActor.props)

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    val classifier1 = MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    val classifier2 = MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(classifier1), probe1.ref)
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(classifier2), probe2.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
    probe2.expectMsg(msgFromPeer)

    peerMessageBusActor.tell(PeerMessageBusActor.Unsubscribe(classifier1), probe1.ref)

    val msgFromPeer2 = MessageFromPeer(Ping(), PeerId("99"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
    probe2.expectMsg(msgFromPeer2)
  }

  it should "only relay matching message codes" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerMessageBusActor.props)

    val probe1 = TestProbe()
    val classifier1 = MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(classifier1), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)

    val msgFromPeer2 = MessageFromPeer(Pong(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
  }

  it should "relay a single notification when subscribed twice to the same message code" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerMessageBusActor.props)

    val probe1 = TestProbe()
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code, Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
    probe1.expectNoMsg()
  }

  it should "allow to handle subscriptions using AllPeers and WithId PeerSelector at the same time" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerMessageBusActor.props)

    val probe1 = TestProbe()
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer)

    // Receive a single notification
    probe1.expectMsg(msgFromPeer)
    probe1.expectNoMsg()

    val msgFromPeer2 = MessageFromPeer(Ping(), PeerId("2"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer2)

    // Receive based on AllPeers subscription
    probe1.expectMsg(msgFromPeer2)

    peerMessageBusActor.tell(PeerMessageBusActor.Unsubscribe(MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)), probe1.ref)
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer)

    // Still received after unsubscribing from AllPeers
    probe1.expectMsg(msgFromPeer)
  }

  it should "allow to subscribe to new messages" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerMessageBusActor.props)

    val probe1 = TestProbe()
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer = MessageFromPeer(Pong(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
  }

  it should "not change subscriptions when subscribing to empty set" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerMessageBusActor.props)

    val probe1 = TestProbe()
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
  }

  it should "allow to unsubscribe from messages" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerMessageBusActor.props)

    val probe1 = TestProbe()
    peerMessageBusActor.tell(PeerMessageBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    val msgFromPeer1 = MessageFromPeer(Ping(), PeerId("1"))
    val msgFromPeer2 = MessageFromPeer(Pong(), PeerId("1"))
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer1)
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer2)

    probe1.expectMsg(msgFromPeer1)
    probe1.expectMsg(msgFromPeer2)

    peerMessageBusActor.tell(PeerMessageBusActor.Unsubscribe(MessageClassifier(Set(Pong.code), PeerSelector.WithId(PeerId("1")))), probe1.ref)

    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer1)
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer2)

    probe1.expectMsg(msgFromPeer1)
    probe1.expectNoMsg()

    peerMessageBusActor.tell(PeerMessageBusActor.Unsubscribe(), probe1.ref)

    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer1)
    peerMessageBusActor ! PeerMessageBusActor.Publish(msgFromPeer2)

    probe1.expectNoMsg()
  }

}

