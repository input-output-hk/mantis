package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.{EtcPeerManagerActor, PeerId}
import io.iohk.ethereum.utils.Config.SyncConfig
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

class FastSyncPivotBlockSelector(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {

  import FastSyncPivotBlockSelector._
  import syncConfig._

  val fastSync: ActorRef = context.parent

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  override def receive: Receive = idle

  def idle: Receive = handleCommonMessages orElse { case ChoosePivotBlock =>
    val peersUsedToChooseTarget = peersToDownloadFrom.collect { case (peer, PeerInfo(_, _, true, maxBlockNumber, _)) =>
      (peer, maxBlockNumber)
    }

    val peersSortedByBestNumber = peersUsedToChooseTarget.toList.sortBy { case (_, number) => -number }
    val bestPeerBestBlockNumber = peersSortedByBestNumber.headOption
      .map { case (_, bestPeerBestBlockNumber) => bestPeerBestBlockNumber }
      .getOrElse(BigInt(0))
    val expectedPivotBlock = (bestPeerBestBlockNumber - syncConfig.pivotBlockOffset).max(0)
    val correctPeers = peersSortedByBestNumber
      .takeWhile { case (_, number) => number >= expectedPivotBlock }
      .map { case (peer, _) => peer }

    if (correctPeers.size >= minPeersToChoosePivotBlock) {

      val (peersToAsk, waitingPeers) = correctPeers.splitAt(minPeersToChoosePivotBlock + peersToChoosePivotBlockMargin)

      log.info(
        "Trying to choose fast sync pivot block using {} peers ({} correct ones). Ask {} peers for block nr {}",
        peersUsedToChooseTarget.size,
        correctPeers.size,
        peersToAsk.size,
        expectedPivotBlock
      )

      peersToAsk.foreach(peer => obtainBlockHeaderFromPeer(peer.id, expectedPivotBlock))

      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, PivotBlockTimeout)
      context become waitingForPivotBlock(
        peersToAsk.map(_.id).toSet,
        waitingPeers.map(_.id),
        expectedPivotBlock,
        timeout,
        Map.empty
      )
    } else {
      log.info(
        "Cannot pick pivot block. Need at least {} peers, but there are only {} which meet the criteria ({} all available at the moment). Retrying in {}",
        minPeersToChoosePivotBlock,
        correctPeers.size,
        peersUsedToChooseTarget.size,
        startRetryInterval
      )
      scheduleRetry(startRetryInterval)
    }
  }

  def waitingForPivotBlock(
      peersToAsk: Set[PeerId],
      waitingPeers: List[PeerId],
      pivotBlockNumber: BigInt,
      timeout: Cancellable,
      headers: Map[ByteString, BlockHeaderWithVotes]
  ): Receive =
    handleCommonMessages orElse {
      case MessageFromPeer(blockHeaders: BlockHeaders, peerId) =>
        peerEventBus ! Unsubscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peerId)))
        val updatedPeersToAsk = peersToAsk - peerId
        val targetBlockHeaderOpt =
          if (blockHeaders.headers.size != 1) None
          else
            blockHeaders.headers.find(header => header.number == pivotBlockNumber)
        targetBlockHeaderOpt match {
          case Some(targetBlockHeader) =>
            val newValue =
              headers.get(targetBlockHeader.hash).map(_.vote).getOrElse(BlockHeaderWithVotes(targetBlockHeader))
            val updatedHeaders = headers.updated(targetBlockHeader.hash, newValue)
            votingProcess(updatedPeersToAsk, waitingPeers, pivotBlockNumber, timeout, updatedHeaders)
          case None =>
            blacklist(peerId, blacklistDuration, "Did not respond with pivot block header, blacklisting")
            votingProcess(updatedPeersToAsk, waitingPeers, pivotBlockNumber, timeout, headers)
        }
      case PivotBlockTimeout =>
        peersToAsk.foreach { peerId =>
          blacklist(peerId, blacklistDuration, "Did not respond with pivot block header (timeout), blacklisting")
        }
        peerEventBus ! Unsubscribe()
        log.info("Pivot block header receive timeout. Retrying in {}", startRetryInterval)
        scheduleRetry(startRetryInterval)
    }

  private def votingProcess(
      peersToAsk: Set[PeerId],
      waitingPeers: List[PeerId],
      pivotBlockNumber: BigInt,
      timeout: Cancellable,
      headers: Map[ByteString, BlockHeaderWithVotes]
  ): Unit = {
    val BlockHeaderWithVotes(mostPopularBlockHeader, updatedVotes) = headers.mostVotedHeader
    // All peers responded - consensus reached
    if (peersToAsk.isEmpty && updatedVotes >= minPeersToChoosePivotBlock) {
      timeout.cancel()
      sendResponseAndCleanup(mostPopularBlockHeader)
      // Consensus could not be reached - ask additional peer if available
    } else if (!isPossibleToReachConsensus(peersToAsk.size, updatedVotes)) {
      timeout.cancel()
      if (waitingPeers.nonEmpty) { // There are more peers to ask
        val newTimeout = scheduler.scheduleOnce(peerResponseTimeout, self, PivotBlockTimeout)
        val additionalPeer :: newWaitingPeers = waitingPeers

        obtainBlockHeaderFromPeer(additionalPeer, pivotBlockNumber)

        context become waitingForPivotBlock(
          peersToAsk + additionalPeer,
          newWaitingPeers,
          pivotBlockNumber,
          newTimeout,
          headers
        )
      } else { // No more peers. Restart the whole process
        peerEventBus ! Unsubscribe()
        log.info("Not enough votes for pivot block. Retrying in {}", startRetryInterval)
        scheduleRetry(startRetryInterval)
      }
      // Continue voting
    } else {
      context become waitingForPivotBlock(
        peersToAsk,
        waitingPeers,
        pivotBlockNumber,
        timeout,
        headers
      )
    }
  }

  private def isPossibleToReachConsensus(peersLeft: Int, bestHeaderVotes: Int): Boolean =
    peersLeft + bestHeaderVotes >= minPeersToChoosePivotBlock

  def scheduleRetry(interval: FiniteDuration): Unit = {
    scheduler.scheduleOnce(interval, self, ChoosePivotBlock)
    context become idle
  }

  def sendResponseAndCleanup(pivotBlockHeader: BlockHeader): Unit = {
    log.info("Found pivot block: {} hash: {}", pivotBlockHeader.number, pivotBlockHeader.hashAsHexString)
    fastSync ! Result(pivotBlockHeader)
    peerEventBus ! Unsubscribe()
    context stop self
  }

  private def obtainBlockHeaderFromPeer(peer: PeerId, blockNumber: BigInt): Unit = {
    peerEventBus ! Subscribe(MessageClassifier(Set(BlockHeaders.code), PeerSelector.WithId(peer)))
    etcPeerManager ! EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(blockNumber), 1, 0, reverse = false),
      peer
    )
  }
}

object FastSyncPivotBlockSelector {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props =
    Props(new FastSyncPivotBlockSelector(etcPeerManager: ActorRef, peerEventBus, syncConfig, scheduler))

  case object ChoosePivotBlock
  case class Result(targetBlockHeader: BlockHeader)

  case object PivotBlockTimeout

  case class BlockHeaderWithVotes(header: BlockHeader, votes: Int = 1) {
    def vote: BlockHeaderWithVotes = copy(votes = votes + 1)
  }

  implicit class SortableHeadersMap(headers: Map[ByteString, BlockHeaderWithVotes]) {
    def mostVotedHeader: BlockHeaderWithVotes = headers.maxBy { case (_, headerWithVotes) =>
      headerWithVotes.votes
    }._2
  }
}
