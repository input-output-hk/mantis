package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.SyncController.DependencyActors
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.network.PeerActor.{Status => PeerStatus}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.{Peer, PeerActor, PeerManagerActor}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.validators.Validators

class SyncController(
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val blockchainStorages: BlockchainStorages,
    val fastSyncStateStorage: FastSyncStateStorage,
    val ledger: Ledger,
    val validators: Validators,
    val actors: DependencyActors,
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

  var handshakedPeers: Map[Peer, PeerStatus.Handshaked] = Map.empty

  scheduler.schedule(0.seconds, peersScanInterval, actors.peerManager, PeerManagerActor.GetPeers)

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
    case peers: PeerManagerActor.Peers =>
      peers.peers.foreach {
        case (peer, _: PeerActor.Status.Handshaked) =>
          if (!handshakedPeers.contains(peer)) context watch peer.ref

        case (peer, _) if handshakedPeers.contains(peer) =>
          removePeer(peer.ref)

        case _ => // nothing
      }

      handshakedPeers = peers.handshaked

    case Terminated(ref) if handshakedPeers.exists(_._1.ref == ref) =>
      removePeer(ref)

    case BlacklistPeer(ref, reason) =>
      blacklist(ref, blacklistDuration, reason)

    case UnblacklistPeer(ref) =>
      undoBlacklist(ref)
  }

  def removePeer(peerRef: ActorRef): Unit = {
    context.unwatch(peerRef)
    handshakedPeers.find(_._1.ref == peerRef).foreach { case (peer, _) => undoBlacklist(peer.id) }
    handshakedPeers = handshakedPeers.filterNot(_._1.ref == peerRef)
  }

  def peersToDownloadFrom: Map[Peer, PeerStatus.Handshaked] =
    handshakedPeers.filterNot { case (p, s) => isBlacklisted(p.id) }
}

object SyncController {
  def props(appStateStorage: AppStateStorage,
            blockchain: Blockchain,
            blockchainStorages: BlockchainStorages,
            syncStateStorage: FastSyncStateStorage,
            ledger: Ledger,
            validators: Validators,
            actors: DependencyActors):
  Props = Props(new SyncController(appStateStorage, blockchain, blockchainStorages,
    syncStateStorage, ledger, validators, actors))

  case class DependencyActors(
    peerManager: ActorRef,
    peerMessageBus: ActorRef,
    pendingTransactionsManager: ActorRef,
    ommersPool: ActorRef)

  case class BlockHeadersToResolve(peer: Peer, headers: Seq[BlockHeader])

  case class BlockHeadersReceived(peer: Peer, headers: Seq[BlockHeader])

  case class BlockBodiesReceived(peer: Peer, requestedHashes: Seq[ByteString], bodies: Seq[BlockBody])

  case class MinedBlock(block: Block)

  case object StartSync

  case object PrintStatus

  case object FastSyncDone
}
