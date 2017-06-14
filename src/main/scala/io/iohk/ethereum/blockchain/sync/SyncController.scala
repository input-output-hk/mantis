package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.PeerDisconnectedClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.validators.Validators

class SyncController(
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val blockchainStorages: BlockchainStorages,
    val fastSyncStateStorage: FastSyncStateStorage,
    val ledger: Ledger,
    val validators: Validators,
    val peerEventBus: ActorRef,
    val pendingTransactionsManager: ActorRef,
    val ommersPool: ActorRef,
    val etcPeerManager: ActorRef,
    val externalSchedulerOpt: Option[Scheduler] = None)
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

  var handshakedPeers: Map[Peer, PeerInfo] = Map.empty

  scheduler.schedule(0.seconds, peersScanInterval, etcPeerManager, EtcPeerManagerActor.GetHandshakedPeers)

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
    case EtcPeerManagerActor.HandshakedPeers(peers) =>
      peers.foreach {
        case (peer, _) if !handshakedPeers.contains(peer) =>
          peerEventBus ! Subscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peer.id)))

        case _ => // nothing
      }

      handshakedPeers = peers

    case PeerDisconnected(peerId) if handshakedPeers.exists(_._1.id == peerId) =>
      removePeer(peerId)

    case BlacklistPeer(ref, reason) =>
      blacklist(ref, blacklistDuration, reason)

    case UnblacklistPeer(ref) =>
      undoBlacklist(ref)
  }

  def removePeer(peerId: PeerId): Unit = {
    peerEventBus ! Unsubscribe(PeerDisconnectedClassifier(PeerSelector.WithId(peerId)))
    handshakedPeers.find(_._1.id == peerId).foreach { case (peer, _) => undoBlacklist(peer.id) }
    handshakedPeers = handshakedPeers.filterNot(_._1.id == peerId)
  }

  def peersToDownloadFrom: Map[Peer, PeerInfo] =
    handshakedPeers.filterNot { case (p, s) => isBlacklisted(p.id) }
}

object SyncController {
  //todo refactor to provide ActorRefs differently
  // scalastyle:off parameter.number
  def props(appStateStorage: AppStateStorage,
            blockchain: Blockchain,
            blockchainStorages: BlockchainStorages,
            syncStateStorage: FastSyncStateStorage,
            ledger: Ledger,
            validators: Validators,
            peerEventBus: ActorRef,
            pendingTransactionsManager: ActorRef,
            ommersPool: ActorRef,
            peersInfoHolder: ActorRef):
  Props = Props(new SyncController(appStateStorage, blockchain, blockchainStorages, syncStateStorage, ledger, validators,
    peerEventBus, pendingTransactionsManager, ommersPool, peersInfoHolder))

  case class DependencyActors(
    )

  case class BlockHeadersToResolve(peer: Peer, headers: Seq[BlockHeader])

  case class BlockHeadersReceived(peer: Peer, headers: Seq[BlockHeader])

  case class BlockBodiesReceived(peer: Peer, requestedHashes: Seq[ByteString], bodies: Seq[BlockBody])

  case class MinedBlock(block: Block)

  case object StartSync

  case object PrintStatus

  case object FastSyncDone
}
