package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerHandshaked}
import io.iohk.ethereum.network.PeerManagerActor.{ConnectToPeer, GetPeers, Peers}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class NetworkImplSpec extends FlatSpec with Matchers {

  it should "appropriately respond to the request for peers" in new TestSetup {
    val futurePeers = network.peers()
    peerManager.expectMsg(GetPeers)
    peerManager.reply(defaultPeers)
    Await.result(futurePeers, 3.seconds) shouldBe defaultPeers
  }

  it should "appropriately send the request for connecting a peer" in new TestSetup {
    val futurePeers = network.connect(uri)
    peerManager.expectMsg(ConnectToPeer(uri))
  }

  it should "appropriately start listening if necessary" in new TestSetup {
    val futurePeers = network.listen(new InetSocketAddress(uri.getHost, uri.getPort))
    server.expectMsg(ServerActor.StartServer(new InetSocketAddress(uri.getHost, uri.getPort)))
  }

  it should "appropriately subscribe to a set of messages sent by any peer" in new TestSetup {
    val sender = TestProbe()

    network.subscribe(defaultSetOfMsgs)(sender.ref)
    peerEventBus.expectMsg(PeerEventBusActor.Subscribe(MessageClassifier(defaultSetOfMsgs, PeerSelector.AllPeers)))
    val subscriber = peerEventBus.sender()

    subscriber shouldBe sender.ref
  }

  it should "appropriately unsubscribe from a set of messages sent by any peer" in new TestSetup {
    val sender = TestProbe()

    network.unsubscribe(defaultSetOfMsgs)(sender.ref)
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

    val uri = new URI("enode://18a551bee469c2e02de660ab01dede06503c986f6b8520cb5a65ad122df88b17b285e3fef09a40a0d44f99e014f8616cf1ebc2e094f96c6e09e2f390f5d34857@47.90.36.129:30303")

    val peerManager = TestProbe()
    val peerEventBus = TestProbe()
    val server = TestProbe()

    val network: Network = new NetworkImpl(peerManager.ref, peerEventBus.ref, server.ref)

    val peer1 = TestProbe()
    val peer2 = TestProbe()

    val defaultPeers = Peers(Map(
      PeerImpl(new InetSocketAddress("127.0.0.1", 0), peer1.ref, peerEventBus.ref) -> PeerActor.Status.Handshaking(0),
      PeerImpl(new InetSocketAddress("127.0.0.1", 1), peer2.ref, peerEventBus.ref) -> PeerActor.Status.Connecting
    ))

    val defaultSetOfMsgs = Set(Ping.code, Pong.code)
  }
}
