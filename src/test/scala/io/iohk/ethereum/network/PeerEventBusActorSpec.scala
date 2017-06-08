package io.iohk.ethereum.network

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageReceivedClassifier
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

class PeerEventBusActorSpec extends FlatSpec with Matchers {

  "PeerEventBusActor" should "relay messages to subscribers" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerEventBusActor.props)

    val probe1 = TestProbe()
    val probe2 = TestProbe()
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

  it should "only relay matching message codes" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerEventBusActor.props)

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

}
