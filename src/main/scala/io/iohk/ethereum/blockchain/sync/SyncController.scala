package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.network.PeerActor.{Status => PeerStatus}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.{PeerActor, PeerManagerActor}
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.validators.Validators

class SyncController(
    val peerManager: ActorRef,
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

  var handshakedPeers: Map[ActorRef, PeerStatus.Handshaked] = Map.empty

  scheduler.schedule(0.seconds, peersScanInterval, peerManager, PeerManagerActor.GetPeers)

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
          if (!handshakedPeers.contains(peer.ref)) context watch peer.ref

        case (peer, _) if handshakedPeers.contains(peer.ref) =>
          removePeer(peer.ref)

        case _ => // nothing
      }

      handshakedPeers = peers.handshaked.map { case (k, v) => (k.ref, v) }

    case Terminated(ref) if handshakedPeers.contains(ref) =>
      removePeer(ref)

    case BlacklistPeer(ref, reason) =>
      blacklist(ref, blacklistDuration, reason)

    case UnblacklistPeer(ref) =>
      undoBlacklist(ref)
  }

  def removePeer(peer: ActorRef): Unit = {
    context.unwatch(peer)
    undoBlacklist(peer)
    handshakedPeers -= peer
  }

  def peersToDownloadFrom: Map[ActorRef, PeerStatus.Handshaked] =
    handshakedPeers.filterNot { case (p, s) => isBlacklisted(p) }
}

object SyncController {
  def props(peerManager: ActorRef,
            appStateStorage: AppStateStorage,
            blockchain: Blockchain,
            blockchainStorages: BlockchainStorages,
            syncStateStorage: FastSyncStateStorage,
            ledger: Ledger,
            validators: Validators):
  Props = Props(new SyncController(peerManager, appStateStorage, blockchain, blockchainStorages, syncStateStorage, ledger, validators))

  case class BlockHeadersToResolve(peer: ActorRef, headers: Seq[BlockHeader])

  case class BlockHeadersReceived(peer: ActorRef, headers: Seq[BlockHeader])

  case class BlockBodiesReceived(peer: ActorRef, requestedHashes: Seq[ByteString], bodies: Seq[BlockBody])

  case object StartSync

  case object PrintStatus

  case object FastSyncDone
}
