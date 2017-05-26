package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.testkit.TestProbe
import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerDisconnected, PeerHandshakeSuccessful, PeerStatusUpdated}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.{MessageClassifier, PeerDisconnectedClassifier, PeerHandshaked, PeerStatusUpdate}
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerManagerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong}
import org.scalatest.{FlatSpec, Matchers}

import scala.language.postfixOps
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

class PeerMessageBusActorSpec extends FlatSpec with Matchers {

  "PeerMessageBusActor" should "relay messages to subscribers" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerEventBusActor.props)

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    val classifier1 = MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    val classifier2 = MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)
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
    val classifier1 = MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(classifier1), probe1.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)

    val msgFromPeer2 = MessageFromPeer(Pong(), PeerId("1"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgFromPeer2)
    probe1.expectNoMsg()
  }

  it should "relay peers disconnecting to its subscribers" in {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerEventBusActor.props)

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerId("1"))), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerId("2"))), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerId("2"))), probe2.ref)

    val msgPeerDisconnected = PeerDisconnected(PeerId("2"))
    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)

    probe1.expectMsg(msgPeerDisconnected)
    probe2.expectMsg(msgPeerDisconnected)

    peerMessageBusActor.tell(PeerEventBusActor.Unsubscribe(PeerDisconnectedClassifier(PeerId("2"))), probe1.ref)

    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)
    probe1.expectNoMsg()
    probe2.expectMsg(msgPeerDisconnected)
  }

  it should "relay peers handshaked to its subscribers" in new TestSetup {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerEventBusActor.props)

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe2.ref)

    val peerHandshaked = PeerImpl(new InetSocketAddress("127.0.0.1", 0), TestProbe().ref, peerMessageBusActor)
    val msgPeerHandshaked = PeerHandshakeSuccessful(peerHandshaked, initialPeerInfo)
    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)

    probe1.expectMsg(msgPeerHandshaked)
    probe2.expectMsg(msgPeerHandshaked)

    peerMessageBusActor.tell(PeerEventBusActor.Unsubscribe(PeerHandshaked), probe1.ref)

    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)
    probe1.expectNoMsg()
    probe2.expectMsg(msgPeerHandshaked)
  }

  it should "relay peers status update to its subscribers" in new TestSetup {
    implicit val system = ActorSystem("test-system")

    val peerMessageBusActor = system.actorOf(PeerEventBusActor.props)

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerStatusUpdate(PeerId("1"))), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerStatusUpdate(PeerId("2"))), probe1.ref)
    peerMessageBusActor.tell(PeerEventBusActor.Subscribe(PeerStatusUpdate(PeerId("2"))), probe2.ref)

    val peerHandshaked = PeerImpl(new InetSocketAddress("127.0.0.1", 0), TestProbe().ref, peerMessageBusActor)
    val msgPeerStatusUpdate = PeerStatusUpdated(PeerId("2"), initialPeerInfo)
    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerStatusUpdate)

    probe1.expectMsg(msgPeerStatusUpdate)
    probe2.expectMsg(msgPeerStatusUpdate)

    peerMessageBusActor.tell(PeerEventBusActor.Unsubscribe(PeerStatusUpdate(PeerId("2"))), probe1.ref)

    peerMessageBusActor ! PeerEventBusActor.Publish(msgPeerStatusUpdate)
    probe1.expectNoMsg()
    probe2.expectMsg(msgPeerStatusUpdate)
  }

  trait TestSetup {

    val peerConf = new PeerConfiguration {
      override val fastSyncHostConfiguration: FastSyncHostConfiguration = new FastSyncHostConfiguration {
        val maxBlocksHeadersPerMessage: Int = 200
        val maxBlocksBodiesPerMessage: Int = 200
        val maxReceiptsPerMessage: Int = 200
        val maxMptComponentsPerMessage: Int = 200
      }
      override val waitForStatusTimeout: FiniteDuration = 30 seconds
      override val waitForChainCheckTimeout: FiniteDuration = 15 seconds
      override val connectMaxRetries: Int = 3
      override val connectRetryDelay: FiniteDuration = 1 second
      override val disconnectPoisonPillTimeout: FiniteDuration = 5 seconds
      override val maxPeers = 10
      override val networkId: Int = 1
    }

    val peerStatus = Status(
      protocolVersion = Message.PV63,
      networkId = peerConf.networkId,
      totalDifficulty = BigInt(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo = EtcPeerInfo(
      remoteStatus = peerStatus,
      totalDifficulty = peerStatus.totalDifficulty,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number
    )
  }

}
