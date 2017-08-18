package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.Sync.{blacklistDuration, minPeersToChooseTargetBlock, peerResponseTimeout, startRetryInterval}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

class FastSyncTargetBlockChooser(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler)
  extends Actor with ActorLogging with PeerListSupport with BlacklistSupport {

  import FastSyncTargetBlockChooser._

  val fastSync: ActorRef = context.parent

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  override def receive: Receive = idle

  def idle: Receive = handleCommonMessages orElse {
    case ChooseTargetBlock =>
      val peersUsedToChooseTarget = peersToDownloadFrom.filter(_._2.forkAccepted)

      if (peersUsedToChooseTarget.size >= minPeersToChooseTargetBlock) {
        peersUsedToChooseTarget.foreach { case (peer, PeerInfo(status, _, _, _)) =>
          peerEventBus ! Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
          etcPeerManager ! EtcPeerManagerActor.SendMessage(GetBlockHeaders(Right(status.bestHash), 1, 0, reverse = false), peer.id)
        }
        log.debug("Asking {} peers for block headers", peersUsedToChooseTarget.size)
        val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, BlockHeadersTimeout)
        context become waitingForBlockHeaders(peersUsedToChooseTarget.keySet, Map.empty, timeout)
      } else {
        log.info("Block synchronization (fast mode) not started. Need at least {} peers, but there are only {} available at the moment. Retrying in {}",
          minPeersToChooseTargetBlock, peersUsedToChooseTarget.size, startRetryInterval)
        scheduleRetry(startRetryInterval)
        context become idle
      }
  }

  def waitingForBlockHeaders(waitingFor: Set[Peer], received: Map[Peer, BlockHeader], timeout: Cancellable): Receive =
    handleCommonMessages orElse {
    case MessageFromPeer(BlockHeaders(Seq(blockHeader)), peerId) =>
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))

      val newWaitingFor = waitingFor.filterNot(_.id == peerId)

      waitingFor.find(_.id == peerId).foreach { peer =>
        val newReceived = received + (peer -> blockHeader)

        if (newWaitingFor.isEmpty) {
          timeout.cancel()
          tryChooseTargetBlock(newReceived)
        } else context become waitingForBlockHeaders(newWaitingFor, newReceived, timeout)
      }

    case MessageFromPeer(BlockHeaders(blockHeaders), peerId) =>
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))
      blacklist(peerId, blacklistDuration,s"did not respond with 1 header but with ${blockHeaders.size}, blacklisting for $blacklistDuration")
      waitingFor.find(_.id == peerId).foreach { peer =>
        context become waitingForBlockHeaders(waitingFor - peer, received, timeout)
      }

    case BlockHeadersTimeout =>
      waitingFor.foreach { peer =>
        peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
        blacklist(peer.id, blacklistDuration, s"did not respond within required time with block header, blacklisting for $blacklistDuration")
      }
      tryChooseTargetBlock(received)
  }

  def tryChooseTargetBlock(receivedHeaders: Map[Peer, BlockHeader]): Unit = {
    log.debug("Trying to choose fast sync target block. Received {} block headers", receivedHeaders.size)
    if (receivedHeaders.size >= minPeersToChooseTargetBlock) {
      val (mostUpToDatePeer, mostUpToDateBlockHeader) = receivedHeaders.maxBy(_._2.number)
      val targetBlock = mostUpToDateBlockHeader.number - syncConfig.targetBlockOffset
      peerEventBus ! Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(mostUpToDatePeer.id)))
      etcPeerManager ! EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(targetBlock), 1, 0, reverse = false), mostUpToDatePeer.id)
      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, TargetBlockTimeout)
      context become waitingForTargetBlock(mostUpToDatePeer, targetBlock, timeout)

    } else {
      log.info("Block synchronization (fast mode) not started. Need to receive block headers from at least {} peers, but received only from {}. Retrying in {}",
        minPeersToChooseTargetBlock, receivedHeaders.size, startRetryInterval)
      scheduleRetry(startRetryInterval)
      context become idle
    }
  }

  def waitingForTargetBlock(peer: Peer, targetBlockNumber: BigInt, timeout: Cancellable): Receive =
    handleCommonMessages orElse {
    case MessageFromPeer(blockHeaders: BlockHeaders, _) =>
      timeout.cancel()
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))

      val targetBlockHeaderOpt = blockHeaders.headers.find(header => header.number == targetBlockNumber)
      targetBlockHeaderOpt match {
        case Some(targetBlockHeader) =>
          sendResponseAndCleanup(targetBlockHeader)
        case None =>
          blacklist(peer.id, blacklistDuration, s"did not respond with target block header, blacklisting and scheduling retry in $startRetryInterval")
          log.info("Block synchronization (fast mode) not started. Target block header not received. Retrying in {}", startRetryInterval)
          scheduleRetry(startRetryInterval)
          context become idle
      }

    case TargetBlockTimeout =>
      blacklist(peer.id, blacklistDuration, s"did not respond with target block header (timeout), blacklisting and scheduling retry in $startRetryInterval")
      log.info("Block synchronization (fast mode) not started. Target block header receive timeout. Retrying in {}", startRetryInterval)
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
      scheduleRetry(startRetryInterval)
      context become idle
  }

  def scheduleRetry(interval: FiniteDuration): Unit = {
    scheduler.scheduleOnce(interval, self, ChooseTargetBlock)
  }

  def sendResponseAndCleanup(targetBlockHeader: BlockHeader): Unit = {
    fastSync ! Result(targetBlockHeader)
    peerEventBus ! Unsubscribe()
    context stop self
  }

}

object FastSyncTargetBlockChooser {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new FastSyncTargetBlockChooser(etcPeerManager: ActorRef, peerEventBus, syncConfig, scheduler))

  case object ChooseTargetBlock
  case class Result(targetBlockHeader: BlockHeader)

  private case object BlockHeadersTimeout
  private case object TargetBlockTimeout
}
