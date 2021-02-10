package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Scheduler}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.PeerDisconnectedClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global // FIXME copied from current impl but not sure global EC is a good choice

trait PeerListSupportNg {
  self: Actor with ActorLogging =>
  import PeerListSupportNg._

  def etcPeerManager: ActorRef
  def peerEventBus: ActorRef
  def blacklist: Blacklist
  def syncConfig: SyncConfig
  def scheduler: Scheduler

  var handshakedPeers: PeersMap = Map.empty

  scheduler.scheduleWithFixedDelay(
    0.seconds,
    syncConfig.peersScanInterval,
    etcPeerManager,
    EtcPeerManagerActor.GetHandshakedPeers
  )(global, context.self)

  def removePeer(peerId: PeerId): Unit = {
    peerEventBus ! Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
    handshakedPeers.find(_._1.id == peerId).foreach { case (peer, _) => blacklist.remove(peer.id) }
    handshakedPeers = handshakedPeers.filterNot(_._1.id == peerId)
  }

  def peersToDownloadFrom: PeersMap =
    handshakedPeers.filterNot { case (p, _) => blacklist.isBlacklisted(p.id) }

  def handlePeerListMessages: Receive = {
    case EtcPeerManagerActor.HandshakedPeers(peers) =>
      peers.keys.filterNot(handshakedPeers.contains).foreach { peer =>
        peerEventBus ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))
      }
      handshakedPeers = peers

    case PeerDisconnected(peerId) if handshakedPeers.exists(_._1.id == peerId) =>
      removePeer(peerId)
  }

  def peerById(peerId: PeerId): Option[Peer] = handshakedPeers collectFirst {
    case (peer, _) if peer.id == peerId => peer
  }

  def blacklistIfHandshaked(peer: Peer, duration: FiniteDuration, reason: String): Unit =
    handshakedPeers.get(peer).foreach(_ => blacklist.add(peer.id, duration, reason))
}

object PeerListSupportNg {
  type PeersMap = Map[Peer, PeerInfo]
}
