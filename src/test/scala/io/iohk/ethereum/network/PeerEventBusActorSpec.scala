package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.PoisonPill
import akka.stream.WatchedActorTerminatedException
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Keep
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.testkit.TestActor
import akka.testkit.TestProbe
import akka.util.ByteString

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.Fixtures
import io.iohk.ethereum.NormalPatience
import io.iohk.ethereum.domain.ChainWeight
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.RemoteStatus
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerHandshakeSuccessful
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier._
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Ping
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong

class PeerEventBusActorSpec extends AnyFlatSpec with Matchers with ScalaFutures with NormalPatience {

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
    probe1.expectNoMessage()
    probe2.expectMsg(msgFromPeer2)

  }

  it should "relay messages via streams" in new TestSetup {
    val classifier1 = MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))
    val classifier2 = MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)

    val peerEventBusProbe = TestProbe()(system)
    peerEventBusProbe.setAutoPilot { (sender: ActorRef, msg: Any) =>
      peerEventBusActor.tell(msg, sender)
      TestActor.KeepRunning
    }

    val seqOnTermination = Flow[MessageFromPeer]
      .recoverWithRetries(1, { case _: WatchedActorTerminatedException => Source.empty })
      .toMat(Sink.seq)(Keep.right)

    val stream1 = PeerEventBusActor.messageSource(peerEventBusProbe.ref, classifier1).runWith(seqOnTermination)
    val stream2 = PeerEventBusActor.messageSource(peerEventBusProbe.ref, classifier2).runWith(seqOnTermination)

    // wait for subscriptions to be done
    peerEventBusProbe.expectMsgType[PeerEventBusActor.Subscribe]
    peerEventBusProbe.expectMsgType[PeerEventBusActor.Subscribe]

    val syncProbe = TestProbe()(system)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(classifier2), syncProbe.ref)

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    val msgFromPeer2 = MessageFromPeer(Ping(), PeerId("99"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    // wait for publications to be done
    syncProbe.expectMsg(msgFromPeer)
    syncProbe.expectMsg(msgFromPeer2)

    peerEventBusProbe.ref ! PoisonPill

    whenReady(stream1)(_ shouldEqual Seq(msgFromPeer))
    whenReady(stream2)(_ shouldEqual Seq(msgFromPeer, msgFromPeer2))
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
    probe1.expectNoMessage()
  }

  it should "relay peers disconnecting to its subscribers" in new TestSetup {

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))),
      probe1.ref
    )
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))),
      probe2.ref
    )

    val msgPeerDisconnected = PeerDisconnected(PeerId("2"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)

    probe1.expectMsg(msgPeerDisconnected)
    probe2.expectMsg(msgPeerDisconnected)

    peerEventBusActor.tell(
      PeerEventBusActor.Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(PeerId("2")))),
      probe1.ref
    )

    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerDisconnected)
    probe1.expectNoMessage()
    probe2.expectMsg(msgPeerDisconnected)
  }

  it should "relay peers handshaked to its subscribers" in new TestSetup {

    val probe1 = TestProbe()
    val probe2 = TestProbe()
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe1.ref)
    peerEventBusActor.tell(PeerEventBusActor.Subscribe(PeerHandshaked), probe2.ref)

    val peerHandshaked =
      new Peer(
        PeerId("peer1"),
        new InetSocketAddress("127.0.0.1", 0),
        TestProbe().ref,
        false,
        nodeId = Some(ByteString())
      )
    val msgPeerHandshaked = PeerHandshakeSuccessful(peerHandshaked, initialPeerInfo)
    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)

    probe1.expectMsg(msgPeerHandshaked)
    probe2.expectMsg(msgPeerHandshaked)

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(PeerHandshaked), probe1.ref)

    peerEventBusActor ! PeerEventBusActor.Publish(msgPeerHandshaked)
    probe1.expectNoMessage()
    probe2.expectMsg(msgPeerHandshaked)
  }

  it should "relay a single notification when subscribed twice to the same message code" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Ping.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
    probe1.expectNoMessage()
  }

  it should "allow to handle subscriptions using AllPeers and WithId PeerSelector at the same time" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)),
      probe1.ref
    )

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    // Receive a single notification
    probe1.expectMsg(msgFromPeer)
    probe1.expectNoMessage()

    val msgFromPeer2 = MessageFromPeer(Ping(), PeerId("2"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    // Receive based on AllPeers subscription
    probe1.expectMsg(msgFromPeer2)

    peerEventBusActor.tell(
      PeerEventBusActor.Unsubscribe(MessageClassifier(Set(Ping.code), PeerSelector.AllPeers)),
      probe1.ref
    )
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    // Still received after unsubscribing from AllPeers
    probe1.expectMsg(msgFromPeer)
  }

  it should "allow to subscribe to new messages" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )

    val msgFromPeer = MessageFromPeer(Pong(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
  }

  it should "not change subscriptions when subscribing to empty set" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )

    val msgFromPeer = MessageFromPeer(Ping(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer)

    probe1.expectMsg(msgFromPeer)
  }

  it should "allow to unsubscribe from messages" in new TestSetup {

    val probe1 = TestProbe()
    peerEventBusActor.tell(
      PeerEventBusActor.Subscribe(MessageClassifier(Set(Ping.code, Pong.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )

    val msgFromPeer1 = MessageFromPeer(Ping(), PeerId("1"))
    val msgFromPeer2 = MessageFromPeer(Pong(), PeerId("1"))
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer1)
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    probe1.expectMsg(msgFromPeer1)
    probe1.expectMsg(msgFromPeer2)

    peerEventBusActor.tell(
      PeerEventBusActor.Unsubscribe(MessageClassifier(Set(Pong.code), PeerSelector.WithId(PeerId("1")))),
      probe1.ref
    )

    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer1)
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    probe1.expectMsg(msgFromPeer1)
    probe1.expectNoMessage()

    peerEventBusActor.tell(PeerEventBusActor.Unsubscribe(), probe1.ref)

    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer1)
    peerEventBusActor ! PeerEventBusActor.Publish(msgFromPeer2)

    probe1.expectNoMessage()
  }

  trait TestSetup {
    implicit val system: ActorSystem = ActorSystem("test-system")

    val peerEventBusActor: ActorRef = system.actorOf(PeerEventBusActor.props)

    val peerStatus: RemoteStatus = RemoteStatus(
      capability = Capability.ETH63,
      networkId = 1,
      chainWeight = ChainWeight.totalDifficultyOnly(10000),
      bestHash = Fixtures.Blocks.Block3125369.header.hash,
      genesisHash = Fixtures.Blocks.Genesis.header.hash
    )
    val initialPeerInfo: PeerInfo = PeerInfo(
      remoteStatus = peerStatus,
      chainWeight = peerStatus.chainWeight,
      forkAccepted = false,
      maxBlockNumber = Fixtures.Blocks.Block3125369.header.number,
      bestBlockHash = peerStatus.bestHash
    )

  }

}
