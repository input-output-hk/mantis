package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier}
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Disconnect, Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

class PeerImplSpec extends FlatSpec with Matchers {

  it should "appropriately send messages to the peer actor" in new TestSetup {
    peer.send(defaultMessage)
    peerActorProbe.expectMsg(PeerActor.SendMessage(defaultMessage))
  }

  it should "appropriately send various messages to the peer actor" in new TestSetup {
    import Disconnect.Reasons._

    peer.send(Seq(Disconnect(Other), Disconnect(UselessPeer), Disconnect(DisconnectRequested)).map(b => b: MessageSerializable))
    peerActorProbe.expectMsg(PeerActor.SendMessage(Disconnect(Other)))
    peerActorProbe.expectMsg(PeerActor.SendMessage(Disconnect(UselessPeer)))
    peerActorProbe.expectMsg(PeerActor.SendMessage(Disconnect(DisconnectRequested)))
  }

  it should "appropriately send disconnects to the peer actor" in new TestSetup {
    peer.disconnectFromPeer(defaultReason)
    peerActorProbe.expectMsg(PeerActor.DisconnectPeer(defaultReason))
  }

  it should "appropriately subscribe to a set of messages sent by a peer" in new TestSetup {
    val sender = TestProbe()

    peer.subscribe(defaultSetOfMsgs)(sender.ref)
    peerEventBusProbe.expectMsg(PeerEventBusActor.Subscribe(MessageClassifier(defaultSetOfMsgs, PeerSelector.WithId(peer.id))))
    val subscribeSender = peerEventBusProbe.sender()

    subscribeSender shouldBe sender.ref
  }

  it should "appropriately unsubscribe from a set of messages sent by a peer" in new TestSetup {
    val sender = TestProbe()

    peer.unsubscribe(defaultSetOfMsgs)(sender.ref)
    peerEventBusProbe.expectMsg(PeerEventBusActor.Unsubscribe(MessageClassifier(defaultSetOfMsgs, PeerSelector.WithId(peer.id))))
    val unsubscribeSender = peerEventBusProbe.sender()

    unsubscribeSender shouldBe sender.ref
  }

  it should "appropriately subscribe to the disconnection of a peer" in new TestSetup {
    val sender = TestProbe()

    peer.subscribeToDisconnect()(sender.ref)
    peerEventBusProbe.expectMsg(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(peer.id)))
    val subscribeSender = peerEventBusProbe.sender()

    subscribeSender shouldBe sender.ref
  }

  it should "appropriately unsubscribe to the disconnection of a peer" in new TestSetup {
    val sender = TestProbe()

    peer.unsubscribeFromDisconnect()(sender.ref)
    peerEventBusProbe.expectMsg(PeerEventBusActor.Unsubscribe(PeerDisconnectedClassifier(peer.id)))
    val unsubscribeSender = peerEventBusProbe.sender()

    unsubscribeSender shouldBe sender.ref
  }

  trait TestSetup {

    implicit val system = ActorSystem("PeerImpl_System")

    val peerActorProbe = TestProbe()
    val peerEventBusProbe = TestProbe()

    val peer: Peer = PeerImpl(new InetSocketAddress("127.0.0.1", 0), peerActorProbe.ref, peerEventBusProbe.ref)

    val defaultMessage = Ping()
    val defaultReason = Disconnect.Reasons.Other
    val defaultSetOfMsgs = Set(Ping.code, Pong.code)
  }

}
