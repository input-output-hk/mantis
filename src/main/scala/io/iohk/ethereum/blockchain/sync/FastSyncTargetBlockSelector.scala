package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.SyncConfig
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.ExecutionContext.Implicits.global

class FastSyncTargetBlockSelector(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler)
  extends Actor with ActorLogging with PeerListSupport with BlacklistSupport {

  import FastSyncTargetBlockSelector._
  import syncConfig._

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
        log.info("Cannot pick target block. Need at least {} peers, but there are only {} available at the moment. Retrying in {}",
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

      val (_, mostUpToDateBlockHeader) = receivedHeaders.maxBy(_._2.number)
      val targetBlock = mostUpToDateBlockHeader.number - syncConfig.targetBlockOffset

      val peersToAsk = receivedHeaders.toList.sortBy(-_._2.number)
        .takeWhile { case (_, header) => header.number > targetBlock }
        .map(_._1).take(3) // todo ???

      peersToAsk.foreach{
        peer =>
          peerEventBus ! Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
          etcPeerManager ! EtcPeerManagerActor.SendMessage(GetBlockHeaders(Left(targetBlock), 1, 0, reverse = false), peer.id)
      }

      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, TargetBlockTimeout)
      context become waitingForTargetBlock(peersToAsk.map(_.id).toSet, targetBlock, timeout, Map.empty)

    } else {
      log.info("Cannot pick target block. Need to receive block headers from at least {} peers, but received only from {}. Retrying in {}",
        minPeersToChooseTargetBlock, receivedHeaders.size, startRetryInterval)
      scheduleRetry(startRetryInterval)
      context become idle
    }
  }

  def waitingForTargetBlock(
                             peersToAsk: Set[PeerId],
                             targetBlockNumber: BigInt,
                             timeout: Cancellable,
                             headers: Map[BlockHeader, Int]
                           ): Receive =
    handleCommonMessages orElse {
    case MessageFromPeer(blockHeaders: BlockHeaders, peerId) =>
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))

      val updatedPeersToAsk = peersToAsk - peerId

      val targetBlockHeaderOpt = blockHeaders.headers.find(header => header.number == targetBlockNumber)
      targetBlockHeaderOpt match {
        case Some(targetBlockHeader) if updatedPeersToAsk.isEmpty =>
          timeout.cancel()
          val newValue = headers.find(_._1 == targetBlockHeader).map(_._2).getOrElse(1)
          val mostPopularBlockHeader = headers.updated(targetBlockHeader, newValue).maxBy(_._2)._1
          sendResponseAndCleanup(mostPopularBlockHeader)
        case Some(targetBlockHeader) =>
          val newValue = headers.find(_._1 == targetBlockHeader).map(_._2).getOrElse(1)
          val updatedHeaders = headers.updated(targetBlockHeader, newValue)
          context become waitingForTargetBlock(updatedPeersToAsk, targetBlockNumber, timeout, updatedHeaders)
        case None =>
          blacklist(peerId, blacklistDuration, "Did not respond with target block header, blacklisting")
          context become waitingForTargetBlock(updatedPeersToAsk, targetBlockNumber, timeout, headers)
      }

    case TargetBlockTimeout =>
      peersToAsk.foreach{
        peerId =>
          blacklist(peerId, blacklistDuration, "Did not respond with target block header (timeout), blacklisting")
          peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))
      }
      log.info("Target block header receive timeout. Retrying in {}", startRetryInterval)
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

object FastSyncTargetBlockSelector {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new FastSyncTargetBlockSelector(etcPeerManager: ActorRef, peerEventBus, syncConfig, scheduler))

  case object ChooseTargetBlock
  case class Result(targetBlockHeader: BlockHeader)

  private case object BlockHeadersTimeout
  private case object TargetBlockTimeout
}
