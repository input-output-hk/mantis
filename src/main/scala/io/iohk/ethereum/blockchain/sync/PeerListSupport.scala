package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Scheduler}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.PeerDisconnectedClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait PeerListSupport {
  self: Actor with ActorLogging with BlacklistSupport =>

  def etcPeerManager: ActorRef
  def peerEventBus: ActorRef
  def syncConfig: SyncConfig
  def scheduler: Scheduler

  var handshakedPeers: Map[Peer, PeerInfo] = Map.empty

  scheduler.schedule(0.seconds, syncConfig.peersScanInterval, etcPeerManager, EtcPeerManagerActor.GetHandshakedPeers)(global, context.self)

  def removePeer(peerId: PeerId): Unit = {
    peerEventBus ! Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
    handshakedPeers.find(_._1.id == peerId).foreach { case (peer, _) => undoBlacklist(peer.id) }
    handshakedPeers = handshakedPeers.filterNot(_._1.id == peerId)
  }

  def peersToDownloadFrom: Map[Peer, PeerInfo] =
    handshakedPeers.filterNot { case (p, s) => isBlacklisted(p.id) }

  def handlePeerListMessages: Receive = {
    case EtcPeerManagerActor.HandshakedPeers(peers) =>
      peers.keys.filterNot(handshakedPeers.contains).foreach { peer =>
        peerEventBus ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))
      }
      handshakedPeers = peers

    case PeerDisconnected(peerId) if handshakedPeers.exists(_._1.id == peerId) =>
      removePeer(peerId)
  }
}
