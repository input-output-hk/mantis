package io.iohk.ethereum.blockchain.sync

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.{Block, BlockHeader, Blockchain}
import io.iohk.ethereum.network.PeerActor.{Status => PeerStatus}
import io.iohk.ethereum.network.p2p.messages.PV62.BlockBody
import io.iohk.ethereum.network.p2p.validators.BlockValidator.BlockError
import io.iohk.ethereum.network.{PeerActor, PeerManagerActor}
import io.iohk.ethereum.utils.Config

class SyncController(
    val peerManager: ActorRef,
    val appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    val mptNodeStorage: MptNodeStorage,
    val fastSyncStateStorage: FastSyncStateStorage,
    val blockValidator: (BlockHeader, BlockBody) => Either[BlockError, Block],
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
    case PeerManagerActor.PeersResponse(peers) =>
      peers.foreach(_.ref ! PeerActor.GetStatus)

    case PeerActor.StatusResponse(status: PeerStatus.Handshaked) =>
      if (!handshakedPeers.contains(sender()) && !isBlacklisted(sender())) {
        handshakedPeers += (sender() -> status)
        context watch sender()
      }

    case PeerActor.StatusResponse(_) =>
      removePeer(sender())

    case Terminated(ref) if handshakedPeers.contains(ref) =>
      removePeer(ref)

    case BlacklistPeer(ref) =>
      blacklist(ref, blacklistDuration)

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
            mptNodeStorage: MptNodeStorage,
            syncStateStorage: FastSyncStateStorage,
            blockValidator: (BlockHeader, BlockBody) => Either[BlockError, Block]):
  Props = Props(new SyncController(peerManager, appStateStorage, blockchain, mptNodeStorage, syncStateStorage, blockValidator))

  case object StartSync

  case object FastSyncDone
}
