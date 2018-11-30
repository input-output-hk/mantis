package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{ Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler }
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.{ BlacklistSupport, PeerListSupport }
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{ PeerSelector, Subscribe, Unsubscribe }
import io.iohk.ethereum.network.p2p.messages.PV62.{ BlockHeaders, GetBlockHeaders }
import io.iohk.ethereum.network.{ EtcPeerManagerActor, Peer, PeerId }
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

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
      val peersUsedToChooseTarget = peersToDownloadFrom.filter{ case (_, peerInfo) => peerInfo.forkAccepted }

      if (peersUsedToChooseTarget.size >= minPeersToChooseTargetBlock) {
        peersUsedToChooseTarget.foreach { case (peer, PeerInfo(status, _, _, _)) =>
          subscribeAndSendMsg(peer.id, Right(status.bestHash))
        }

        log.debug("Asking {} peers for block headers", peersUsedToChooseTarget.size)

        val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, BlockHeadersTimeout)
        context become waitingForBlockHeaders(peersUsedToChooseTarget.keySet, Map.empty, timeout)
      } else {
      val message = s"Cannot pick target block. Need at least $minPeersToChooseTargetBlock peers, " +
        s"but there are only ${peersUsedToChooseTarget.size} available at the moment. Retrying in $startRetryInterval"
        scheduleRetry(startRetryInterval, message)
      }
  }

  private def subscribeAndSendMsg(peer: PeerId, block: Either[BigInt, ByteString]): Unit = {
    peerEventBus ! Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer)))
    etcPeerManager ! EtcPeerManagerActor.SendMessage(GetBlockHeaders(block, 1, 0, reverse = false), peer)
  }

  def waitingForBlockHeaders(waitingFor: Set[Peer], received: Map[Peer, BlockHeader], timeout: Cancellable): Receive =
    handleCommonMessages orElse
    handleMessageFromPeer(waitingFor, received, timeout) orElse {
      case BlockHeadersTimeout =>
        waitingFor.foreach { peer =>
          val reason = s"did not respond within required time with block header, blacklisting for $blacklistDuration"
          unsubscribeAndBlacklist(peer.id, reason)
        }
        tryChooseTargetBlock(received)
  }

  private def unsubscribeAndBlacklist(peer: PeerId, reason: String): Unit = {
    peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer)))
    blacklist(peer, blacklistDuration, reason)
  }

  def handleMessageFromPeer(waitingFor: Set[Peer], received: Map[Peer, BlockHeader], timeout: Cancellable): Receive = {
    case MessageFromPeer(BlockHeaders(Seq(blockHeader)), peerId) =>
      peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))

      waitingFor.find(_.id == peerId).foreach { peer =>
        val newReceived = received + (peer -> blockHeader)
        val newWaitingFor = waitingFor.filterNot(_.id == peerId)

        if (newWaitingFor.isEmpty) {
          timeout.cancel()
          tryChooseTargetBlock(newReceived)
        } else context become waitingForBlockHeaders(newWaitingFor, newReceived, timeout)
      }

    case MessageFromPeer(BlockHeaders(blockHeaders), peerId) =>
      val reason = s"did not respond with 1 header but with ${blockHeaders.size}, blacklisting for $blacklistDuration"
      unsubscribeAndBlacklist(peerId, reason)

      waitingFor.find(_.id == peerId).foreach { peer =>
        context become waitingForBlockHeaders(waitingFor - peer, received, timeout)
      }
  }

  def tryChooseTargetBlock(receivedHeaders: Map[Peer, BlockHeader]): Unit = {
    log.debug("Trying to choose fast sync target block. Received {} block headers", receivedHeaders.size)
    if (receivedHeaders.size >= minPeersToChooseTargetBlock) {
      val (mostCurrentPeer, mostCurrentBlockHeader) = receivedHeaders.maxBy{ case (_, header) => header.number }
      val targetBlock = mostCurrentBlockHeader.number - targetBlockOffset
      subscribeAndSendMsg(mostCurrentPeer.id, Left(targetBlock))
      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, TargetBlockTimeout)
      context become waitingForTargetBlock(mostCurrentPeer, targetBlock, timeout)

    } else {
      val message = s"Cannot pick target block. Need to receive block headers from at least " +
        s"$minPeersToChooseTargetBlock peers, but received only from ${receivedHeaders.size}. " +
        s"Retrying in $startRetryInterval"
      scheduleRetry(startRetryInterval, message)
    }
  }

  def waitingForTargetBlock(peer: Peer, targetBlockNumber: BigInt, timeout: Cancellable): Receive =
    handleCommonMessages orElse {
    case MessageFromPeer(blockHeaders: BlockHeaders, _) =>
      timeout.cancel()
      unsubscribe(peer)
      blockHeaders.headers.find(_.number == targetBlockNumber).fold(blacklistAndRetry(peer))(sendResponseAndCleanup)

    case TargetBlockTimeout =>
      val reason = s"did not respond with target block header (timeout), blacklisting and scheduling retry in $startRetryInterval"
      blacklist(peer.id, blacklistDuration, reason)
      unsubscribe(peer)

      val message = s"Target block header receive timeout. Retrying in $startRetryInterval"
      scheduleRetry(startRetryInterval, message)
  }

  private def unsubscribe(peer: Peer): Unit = {
    peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer.id)))
  }

  private def blacklistAndRetry(peer: Peer): Unit = {
    val reason = s"did not respond with target block header, blacklisting and scheduling retry in $startRetryInterval"
    blacklist(peer.id, blacklistDuration, reason)
    val message = s"Target block header not received. Retrying in $startRetryInterval"
    scheduleRetry(startRetryInterval, message)
  }

  def scheduleRetry(interval: FiniteDuration, message: String): Unit = {
    log.info(message)
    scheduler.scheduleOnce(interval, self, ChooseTargetBlock)
    context become idle
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
