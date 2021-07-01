package io.iohk.ethereum.blockchain.sync

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Scheduler

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.PeerSelector
import io.iohk.ethereum.network.PeerEventBusActor.Subscribe
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.PeerDisconnectedClassifier
import io.iohk.ethereum.network.PeerEventBusActor.Unsubscribe
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.utils.Config.SyncConfig

trait PeerListSupportNg { self: Actor with ActorLogging =>
  import PeerListSupportNg._
  import Blacklist._

  implicit private val ec: ExecutionContext = context.dispatcher

  protected val bigIntReverseOrdering: Ordering[BigInt] = Ordering[BigInt].reverse

  def etcPeerManager: ActorRef
  def peerEventBus: ActorRef
  def blacklist: Blacklist
  def syncConfig: SyncConfig
  def scheduler: Scheduler

  protected var handshakedPeers: Map[PeerId, PeerWithInfo] = Map.empty

  scheduler.scheduleWithFixedDelay(
    0.seconds,
    syncConfig.peersScanInterval,
    etcPeerManager,
    EtcPeerManagerActor.GetHandshakedPeers
  )(ec, context.self)

  def handlePeerListMessages: Receive = {
    case EtcPeerManagerActor.HandshakedPeers(peers) => updatePeers(peers)
    case PeerDisconnected(peerId)                   => removePeerById(peerId)
  }

  def peersToDownloadFrom: Map[PeerId, PeerWithInfo] =
    handshakedPeers.filterNot { case (peerId, _) =>
      blacklist.isBlacklisted(peerId)
    }

  def getPeerById(peerId: PeerId): Option[Peer] = handshakedPeers.get(peerId).map(_.peer)

  def getPeerWithHighestBlock: Option[PeerWithInfo] =
    peersToDownloadFrom.values.toList.sortBy(_.peerInfo.maxBlockNumber)(bigIntReverseOrdering).headOption

  def blacklistIfHandshaked(peerId: PeerId, duration: FiniteDuration, reason: BlacklistReason): Unit =
    handshakedPeers.get(peerId).foreach(_ => blacklist.add(peerId, duration, reason))

  private def updatePeers(peers: Map[Peer, PeerInfo]): Unit = {
    val updated = peers.map { case (peer, peerInfo) =>
      (peer.id, PeerWithInfo(peer, peerInfo))
    }
    updated.filterNot(p => handshakedPeers.keySet.contains(p._1)).foreach { case (peerId, _) =>
      peerEventBus ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
    }
    handshakedPeers = updated
  }

  private def removePeerById(peerId: PeerId): Unit =
    if (handshakedPeers.keySet.contains(peerId)) {
      peerEventBus ! Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
      blacklist.remove(peerId)
      handshakedPeers = handshakedPeers - peerId
    }

}

object PeerListSupportNg {
  final case class PeerWithInfo(peer: Peer, peerInfo: PeerInfo)
}
