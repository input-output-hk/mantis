package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, PeerListSupport}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

class PivotBlockSelector(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler,
    fastSync: ActorRef
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {

  import PivotBlockSelector._
  import syncConfig._

  def handleCommonMessages: Receive = handlePeerListMessages.orElse(handleBlacklistMessages)

  override def receive: Receive = idle

  def idle: Receive = handleCommonMessages.orElse { case SelectPivotBlock =>
    val election @ ElectionDetails(correctPeers, expectedPivotBlock) = collectVoters

    if (election.isEnoughVoters(minPeersToChoosePivotBlock)) {

      val (peersToAsk, waitingPeers) = correctPeers.splitAt(minPeersToChoosePivotBlock + peersToChoosePivotBlockMargin)

      log.info(
        "Trying to choose fast sync pivot block using {} peers ({} correct ones). Ask {} peers for block nr {}",
        peersToDownloadFrom.size,
        correctPeers.size,
        peersToAsk.size,
        expectedPivotBlock
      )

      peersToAsk.foreach(peer => obtainBlockHeaderFromPeer(peer.id, expectedPivotBlock))

      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, ElectionPivotBlockTimeout)
      context.become(
        runningPivotBlockElection(
          peersToAsk.map(_.id).toSet,
          waitingPeers.map(_.id),
          expectedPivotBlock,
          timeout,
          Map.empty
        )
      )
    } else {
      log.info(
        "Cannot pick pivot block. Need at least {} peers, but there are only {} which meet the criteria ({} all available at the moment). Retrying in {}",
        minPeersToChoosePivotBlock,
        correctPeers.size,
        peersToDownloadFrom.size,
        startRetryInterval
      )
      scheduleRetry(startRetryInterval)
    }
  }

  def runningPivotBlockElection(
      peersToAsk: Set[PeerId],
      waitingPeers: List[PeerId],
      pivotBlockNumber: BigInt,
      timeout: Cancellable,
      headers: Map[ByteString, BlockHeaderWithVotes]
  ): Receive =
    handleCommonMessages.orElse {
      case MessageFromPeer(blockHeaders: BlockHeaders, peerId) =>
        peerEventBus ! Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peerId)))
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
      case ElectionPivotBlockTimeout =>
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
    // most voted header can return empty if we asked one peer and it returned us non expected block. Then headers map is empty
    // so there is no most voted header
    val maybeBlockHeaderWithVotes = headers.mostVotedHeader
    // All peers responded - consensus reached
    if (peersToAsk.isEmpty && maybeBlockHeaderWithVotes.exists(hWv => hWv.votes >= minPeersToChoosePivotBlock)) {
      timeout.cancel()
      sendResponseAndCleanup(maybeBlockHeaderWithVotes.get.header)
      // Consensus could not be reached - ask additional peer if available
    } else if (!isPossibleToReachConsensus(peersToAsk.size, maybeBlockHeaderWithVotes.map(_.votes).getOrElse(0))) {
      timeout.cancel()
      if (waitingPeers.nonEmpty) { // There are more peers to ask
        val newTimeout = scheduler.scheduleOnce(peerResponseTimeout, self, ElectionPivotBlockTimeout)
        val additionalPeer :: newWaitingPeers = waitingPeers

        obtainBlockHeaderFromPeer(additionalPeer, pivotBlockNumber)

        context.become(
          runningPivotBlockElection(
            peersToAsk + additionalPeer,
            newWaitingPeers,
            pivotBlockNumber,
            newTimeout,
            headers
          )
        )
      } else { // No more peers. Restart the whole process
        peerEventBus ! Unsubscribe()
        log.info("Not enough votes for pivot block. Retrying in {}", startRetryInterval)
        scheduleRetry(startRetryInterval)
      }
      // Continue voting
    } else {
      context.become(
        runningPivotBlockElection(
          peersToAsk,
          waitingPeers,
          pivotBlockNumber,
          timeout,
          headers
        )
      )
    }
  }

  private def isPossibleToReachConsensus(peersLeft: Int, bestHeaderVotes: Int): Boolean =
    peersLeft + bestHeaderVotes >= minPeersToChoosePivotBlock

  def scheduleRetry(interval: FiniteDuration): Unit = {
    scheduler.scheduleOnce(interval, self, SelectPivotBlock)
    context.become(idle)
  }

  def sendResponseAndCleanup(pivotBlockHeader: BlockHeader): Unit = {
    log.info("Found pivot block: {} hash: {}", pivotBlockHeader.number, pivotBlockHeader.hashAsHexString)
    fastSync ! Result(pivotBlockHeader)
    peerEventBus ! Unsubscribe()
    context.stop(self)
  }

  private def obtainBlockHeaderFromPeer(peer: PeerId, blockNumber: BigInt): Unit = {
    peerEventBus ! Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer)))
    etcPeerManager ! EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(blockNumber), 1, 0, reverse = false),
      peer
    )
  }

  private def collectVoters: ElectionDetails = {
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

    ElectionDetails(correctPeers, expectedPivotBlock)
  }
}

object PivotBlockSelector {
  def props(
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      syncConfig: SyncConfig,
      scheduler: Scheduler,
      fastSync: ActorRef
  ): Props =
    Props(new PivotBlockSelector(etcPeerManager: ActorRef, peerEventBus, syncConfig, scheduler, fastSync))

  case object SelectPivotBlock
  case class Result(targetBlockHeader: BlockHeader)

  case object ElectionPivotBlockTimeout

  case class BlockHeaderWithVotes(header: BlockHeader, votes: Int = 1) {
    def vote: BlockHeaderWithVotes = copy(votes = votes + 1)
  }
  import cats.implicits._
  implicit class SortableHeadersMap(headers: Map[ByteString, BlockHeaderWithVotes]) {
    def mostVotedHeader: Option[BlockHeaderWithVotes] =
      headers.toList.maximumByOption { case (_, headerWithVotes) => headerWithVotes.votes }.map(_._2)
  }

  case class ElectionDetails(participants: List[Peer], expectedPivotBlock: BigInt) {
    def isEnoughVoters(minNumberOfVoters: Int): Boolean = participants.size >= minNumberOfVoters
  }
}
