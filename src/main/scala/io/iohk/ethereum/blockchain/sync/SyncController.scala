package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{PeerDisconnected, PeerHandshakeSuccessful, PeerStatusUpdated}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.{Network, Peer, PeerId}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.validators.Validators

class SyncController(
    val network: Network,
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val blockchainStorages: BlockchainStorages,
    val fastSyncStateStorage: FastSyncStateStorage,
    val ledger: Ledger,
    val validators: Validators,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor
    with ActorLogging
    with BlacklistSupport
    with FastSync
    with RegularSync {

  import BlacklistSupport._
  import Config.FastSync._
  import SyncController._

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  var handshakedPeers: Map[PeerId, PeerWithInfo] = Map.empty

  network.subscribeToAnyPeerHandshaked()

  override implicit def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  override def receive: Receive = idle

  def idle: Receive = handlePeerUpdates orElse {
    case StartSync =>
      appStateStorage.putSyncStartingBlock(appStateStorage.getBestBlockNumber())
      scheduler.schedule(0.seconds, printStatusInterval, self, PrintStatus)
      (appStateStorage.isFastSyncDone(), doFastSync) match {
        case (false, true) =>
          startFastSync()
        case (true, true) =>
          log.warning(s"do-fast-sync is set to $doFastSync but fast sync cannot start because it has already been completed")
          startRegularSync()
        case (true, false) =>
          startRegularSync()
        case (false, false) =>
          fastSyncStateStorage.purge()
          startRegularSync()
      }

    case FastSyncDone =>
      startRegularSync()
  }

  def handlePeerUpdates: Receive = {
    case PeerHandshakeSuccessful(peer, initialInfo) =>
      if (!handshakedPeers.contains(peer.id)){
        peer.subscribeToDisconnect()
        peer.subscribeToStatusUpdate()
      }
      handshakedPeers = handshakedPeers + (peer.id -> PeerWithInfo(peer, initialInfo))

    case PeerStatusUpdated(peerId, newPeerInfo: EtcPeerInfo) if handshakedPeers.contains(peerId) =>
      val PeerWithInfo(peer, peerInfo) = handshakedPeers(peerId)
      handshakedPeers = handshakedPeers + (peerId -> PeerWithInfo(peer, newPeerInfo))

    case PeerDisconnected(peerId) if handshakedPeers.contains(peerId) =>
      removePeer(peerId)

    case BlacklistPeer(ref, reason) =>
      blacklist(ref, blacklistDuration, reason)

    case UnblacklistPeer(ref) =>
      undoBlacklist(ref)
  }

  def removePeer(peerId: PeerId): Unit = {
    handshakedPeers.find(_ == peerId).foreach { case (_, PeerWithInfo(handshakedPeer, _)) =>
      handshakedPeer.unsubscribeFromDisconnect()
      handshakedPeer.unsubscribeFromPeerStatusUpdate()
      undoBlacklist(handshakedPeer.id)
    }
    handshakedPeers = handshakedPeers.filterNot(_ == peerId)
  }

  def peersToDownloadFrom: Map[Peer, EtcPeerInfo] =
    handshakedPeers.collect { case (peerId, PeerWithInfo(peer, etcPeerInfo)) if !isBlacklisted(peerId) =>
      peer -> etcPeerInfo
    }
}

object SyncController {
  def props(network: Network,
            appStateStorage: AppStateStorage,
            blockchain: Blockchain,
            blockchainStorages: BlockchainStorages,
            syncStateStorage: FastSyncStateStorage,
            ledger: Ledger,
            validators: Validators):
  Props = Props(new SyncController(network, appStateStorage, blockchain, blockchainStorages,
    syncStateStorage, ledger, validators))

  case class BlockHeadersToResolve(peer: Peer, headers: Seq[BlockHeader])

  case class BlockHeadersReceived(peer: Peer, headers: Seq[BlockHeader])

  case class BlockBodiesReceived(peer: Peer, requestedHashes: Seq[ByteString], bodies: Seq[BlockBody])

  case class MinedBlock(block: Block)

  case object StartSync

  case object PrintStatus

  case object FastSyncDone

  case class PeerWithInfo(peer: Peer, etcPeerInfo: EtcPeerInfo)
}
