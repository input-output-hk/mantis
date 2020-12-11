package io.iohk.ethereum.network

import akka.actor._
import cats._
import cats.implicits._
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.p2p.messages.Codes
import java.time.Clock
import scala.concurrent.duration.FiniteDuration
import cats.kernel.Monoid

class PeerStatisticsActor(
    peerEventBus: ActorRef,
    var maybeStats: Option[TimeSlotStats[PeerId, PeerStatisticsActor.Stat]]
) extends Actor {
  import PeerStatisticsActor._

  // Subscribe to messages received from handshaked peers to maintain stats.
  peerEventBus ! Subscribe(MessageSubscriptionClassifier)
  // Removing peers is an optimisation to free space, but eventualy the stats would be overwritten anyway.
  peerEventBus ! Subscribe(SubscriptionClassifier.PeerDisconnectedClassifier(PeerSelector.AllPeers))

  def receive: Receive = handlePeerEvents orElse handleStatsRequests

  private def handlePeerEvents: Receive = {
    case PeerEvent.MessageFromPeer(_, peerId) =>
      val now = System.currentTimeMillis()
      val obs = Stat(responsesReceived = 1, firstSeenTimeMillis = Some(now), lastSeenTimeMillis = Some(now))
      maybeStats = maybeStats.map(_.add(peerId, obs))

    case PeerEvent.PeerDisconnected(peerId) =>
      maybeStats = maybeStats.map(_.remove(peerId))
  }

  private def handleStatsRequests: Receive = {
    case GetStatsForAll =>
      sender ! StatsForAll(maybeStats.map(_.getAll).getOrElse(Map.empty))

    case GetStatsForPeer(peerId) =>
      sender ! StatsForPeer(peerId, maybeStats.map(_.get(peerId)).getOrElse(Stat.empty))
  }
}

object PeerStatisticsActor {
  case class Stat(
      responsesReceived: Int,
      firstSeenTimeMillis: Option[Long],
      lastSeenTimeMillis: Option[Long]
  )
  object Stat {
    val empty: Stat = Stat(0, None, None)

    private def mergeOpt[A, B](x: A, y: A)(f: A => Option[B])(g: (B, B) => B): Option[B] = {
      val (mx, my) = (f(x), f(y))
      (mx, my).mapN(g) orElse mx orElse my
    }

    implicit val monoid: Monoid[Stat] =
      Monoid.instance(
        empty,
        (a, b) =>
          Stat(
            responsesReceived = a.responsesReceived + b.responsesReceived,
            firstSeenTimeMillis = mergeOpt(a, b)(_.firstSeenTimeMillis)(math.min),
            lastSeenTimeMillis = mergeOpt(a, b)(_.lastSeenTimeMillis)(math.max)
          )
      )
  }

  def props(peerEventBus: ActorRef, slotDuration: FiniteDuration, slotCount: Int): Props =
    Props {
      implicit val clock = Clock.systemUTC()
      new PeerStatisticsActor(peerEventBus, TimeSlotStats[PeerId, Stat](slotDuration, slotCount))
    }

  case object GetStatsForAll
  case class StatsForAll(stats: Map[PeerId, Stat])
  case class GetStatsForPeer(peerId: PeerId)
  case class StatsForPeer(peerId: PeerId, stat: Stat)

  val MessageSubscriptionClassifier =
    SubscriptionClassifier.MessageClassifier(
      // Subscribe to response types, which indicate that we are getting data from that peer.
      messageCodes = Set(
        Codes.NewBlockCode,
        Codes.NewBlockHashesCode,
        Codes.SignedTransactionsCode,
        Codes.BlockHeadersCode,
        Codes.BlockBodiesCode,
        Codes.BlockHashesFromNumberCode,
        Codes.NodeDataCode,
        Codes.ReceiptsCode
      ),
      peerSelector = PeerSelector.AllPeers
    )
}
