package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier, PeerHandshaked}
import io.iohk.ethereum.network.PeerManagerActor.{GetPeers, Peers}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class NetworkImplSpec extends FlatSpec with Matchers {

  it should "appropriately respond to the request for peers" in new TestSetup {
    val futurePeers = network.peersWithStatus()
    peerManager.expectMsg(GetPeers)
    peerManager.reply(defaultPeers)
    Await.result(futurePeers, 3.seconds) shouldBe defaultPeers
  }

  it should "appropriately subscribe to a set of messages sent by any peer" in new TestSetup {
    val sender = TestProbe()

    network.subscribeToSetOfMsgs(defaultSetOfMsgs)(sender.ref)
    peerEventBus.expectMsg(PeerEventBusActor.Subscribe(MessageClassifier(defaultSetOfMsgs, PeerSelector.AllPeers)))
    val subscriber = peerEventBus.sender()

    subscriber shouldBe sender.ref
  }

  it should "appropriately unsubscribe from a set of messages sent by any peer" in new TestSetup {
    val sender = TestProbe()

    network.unsubscribeFromSetOfMsgs(defaultSetOfMsgs)(sender.ref)
    peerEventBus.expectMsg(PeerEventBusActor.Unsubscribe(MessageClassifier(defaultSetOfMsgs, PeerSelector.AllPeers)))
    val subscriber = peerEventBus.sender()

    subscriber shouldBe sender.ref
  }

  it should "appropriately subscribe to the handshaking of any peer" in new TestSetup {
    val sender = TestProbe()

    network.subscribeToAnyPeerHandshaked()(sender.ref)
    peerEventBus.expectMsg(PeerEventBusActor.Subscribe(PeerHandshaked))
    val subscribe = peerEventBus.sender()

    subscribe shouldBe sender.ref
  }

  it should "appropriately unsubscribe to the handshaking of any peer" in new TestSetup {
    val sender = TestProbe()

    network.unsubscribeFromAnyPeerHandshaked()(sender.ref)
    peerEventBus.expectMsg(PeerEventBusActor.Unsubscribe(PeerHandshaked))
    val unsubscriber = peerEventBus.sender()

    unsubscriber shouldBe sender.ref
  }

  it should "appropriately broadcast messages sent to all peers" in new TestSetup {
    val sender = TestProbe()

    network.broadcast(Ping())
    peerManager.expectMsg(GetPeers)
    peerManager.reply(defaultPeers)

    peer1.expectMsg(PeerActor.SendMessage(Ping()))
    peer2.expectMsg(PeerActor.SendMessage(Ping()))
  }

  trait TestSetup {
    implicit val system = ActorSystem("NetworkImpl_System")

    val peerManager = TestProbe()
    val peerEventBus = TestProbe()

    val network: Network = new NetworkImpl(peerManager.ref, peerEventBus.ref)

    val peer1 = TestProbe()
    val peer2 = TestProbe()

    val defaultPeers = Peers(Map(
      PeerImpl(new InetSocketAddress("127.0.0.1", 0), peer1.ref, peerEventBus.ref) -> PeerActor.Status.Handshaking(0),
      PeerImpl(new InetSocketAddress("127.0.0.1", 1), peer2.ref, peerEventBus.ref) -> PeerActor.Status.Connecting
    ))

    val defaultSetOfMsgs = Set(Ping.code, Pong.code)
  }
}
