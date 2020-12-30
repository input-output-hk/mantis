package io.iohk.ethereum.network

import akka.actor._
import akka.testkit.{TestKit, TestProbe}
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.p2p.messages.PV61.NewBlockHashes
import io.iohk.ethereum.WithActorSystemShutDown
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration._

class PeerStatisticsSpec
    extends TestKit(ActorSystem("PeerStatisticsSpec_System"))
    with AnyFlatSpecLike
    with WithActorSystemShutDown
    with Matchers {

  import PeerStatisticsActor._

  behavior of "PeerStatisticsActor"

  it should "subscribe to peer events" in new Fixture {
    peerEventBus.expectMsg(Subscribe(PeerStatisticsActor.MessageSubscriptionClassifier))
    peerEventBus.expectMsg(Subscribe(SubscriptionClassifier.PeerDisconnectedClassifier(PeerSelector.AllPeers)))
  }

  it should "initially return default stats for unknown peers" in new Fixture {
    val peerId = PeerId("Alice")
    peerStatistics ! GetStatsForPeer(1.minute, peerId)
    sender.expectMsg(StatsForPeer(peerId, PeerStat.empty))
  }

  it should "initially return default stats when there are no peers" in new Fixture {
    peerStatistics ! GetStatsForAll(1.minute)
    sender.expectMsg(StatsForAll(Map.empty))
  }

  it should "count received messages" in new Fixture {
    val alice = PeerId("Alice")
    val bob = PeerId("Bob")
    peerStatistics ! PeerEvent.MessageFromPeer(NewBlockHashes(Seq.empty), alice)
    peerStatistics ! PeerEvent.MessageFromPeer(NewBlockHashes(Seq.empty), bob)
    peerStatistics ! PeerEvent.MessageFromPeer(NewBlockHashes(Seq.empty), alice)
    peerStatistics ! GetStatsForAll(1.minute)

    val stats = sender.expectMsgType[StatsForAll]
    stats.stats should not be empty

    val statA = stats.stats(alice)
    statA.responsesReceived shouldBe 2
    // @dv: the corner case when responses are received simultaneously
    // can ruin the tests for no reason
    statA.lastSeenTimeMillis shouldBe >=(statA.firstSeenTimeMillis)

    val statB = stats.stats(bob)
    statB.responsesReceived shouldBe 1
    statB.lastSeenTimeMillis shouldBe statB.firstSeenTimeMillis
  }

  trait Fixture {
    val sender = TestProbe()
    implicit val senderRef = sender.ref

    val peerEventBus = TestProbe()
    val peerStatistics =
      system.actorOf(PeerStatisticsActor.props(peerEventBus.ref, slotDuration = 1.minute, slotCount = 30))
  }
}
