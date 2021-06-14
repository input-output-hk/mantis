package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props, Scheduler}
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason.{
  InvalidPivotBlockElectionResponse,
  PivotBlockElectionTimeout
}
import io.iohk.ethereum.blockchain.sync.PeerListSupportNg.PeerWithInfo
import io.iohk.ethereum.blockchain.sync.{Blacklist, PeerListSupportNg}
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.MessageFromPeer
import io.iohk.ethereum.network.PeerEventBusActor.SubscriptionClassifier.MessageClassifier
import io.iohk.ethereum.network.PeerEventBusActor.{PeerSelector, Subscribe, Unsubscribe}
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.ETH62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerId}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

class PivotBlockSelector(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    val scheduler: Scheduler,
    fastSync: ActorRef,
    val blacklist: Blacklist
) extends Actor
    with ActorLogging
    with PeerListSupportNg {

  import PivotBlockSelector._
  import syncConfig._

  private var pivotBlockRetryCount = 0

  override def receive: Receive = idle

  private def idle: Receive = handlePeerListMessages orElse { case SelectPivotBlock =>
    val electionDetails = collectVoters()
    startPivotBlockSelection(electionDetails)
  }

  private def startPivotBlockSelection(election: ElectionDetails): Unit = {
    val ElectionDetails(correctPeers, currentBestBlockNumber, expectedPivotBlock) = election

    if (election.hasEnoughVoters(minPeersToChoosePivotBlock)) {
      val (peersToAsk, waitingPeers) = correctPeers.splitAt(minPeersToChoosePivotBlock + peersToChoosePivotBlockMargin)

      log.info(
        "Trying to choose fast sync pivot block using {} peers ({} ones with high enough block). Ask {} peers for block nr {}",
        peersToDownloadFrom.size,
        correctPeers.size,
        peersToAsk.size,
        expectedPivotBlock
      )

      peersToAsk.foreach(peer => obtainBlockHeaderFromPeer(peer.id, expectedPivotBlock))

      val timeout = scheduler.scheduleOnce(peerResponseTimeout, self, ElectionPivotBlockTimeout)
      context become runningPivotBlockElection(
        peersToAsk.map(_.id).toSet,
        waitingPeers.map(_.id),
        expectedPivotBlock,
        timeout,
        Map.empty
      )
    } else {
      log.info(
        "Cannot pick pivot block. Need at least {} peers, but there are only {} which meet the criteria " +
          "({} all available at the moment). Best block number = {}",
        minPeersToChoosePivotBlock,
        correctPeers.size,
        peersToDownloadFrom.size,
        currentBestBlockNumber
      )
      retryPivotBlockSelection(currentBestBlockNumber)
    }
  }

  // Voters are collected until minimum peers to choose pivot block is obtained.
  private def retryPivotBlockSelection(pivotBlockNumber: BigInt): Unit = {
    pivotBlockRetryCount += 1
    if (pivotBlockRetryCount <= maxPivotBlockFailuresCount && pivotBlockNumber > 0) {
      val electionDetails = collectVoters(Some(pivotBlockNumber))
      startPivotBlockSelection(electionDetails)
    } else {
      log.debug(
        "Cannot pick pivot block. Current best block number [{}]. Retrying in [{}]",
        pivotBlockNumber,
        startRetryInterval
      )
      // Restart the whole process.
      scheduleRetry(startRetryInterval)
    }
  }

  private def runningPivotBlockElection(
      peersToAsk: Set[PeerId],
      waitingPeers: List[PeerId],
      pivotBlockNumber: BigInt,
      timeout: Cancellable,
      headers: Map[ByteString, BlockHeaderWithVotes]
  ): Receive =
    handlePeerListMessages orElse {
      case MessageFromPeer(blockHeaders: BlockHeaders, peerId) =>
        peerEventBus ! Unsubscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peerId)))
        val updatedPeersToAsk = peersToAsk - peerId
        val targetBlockHeaderOpt = blockHeaders.headers.find(header => header.number == pivotBlockNumber)
        targetBlockHeaderOpt match {
          case Some(targetBlockHeader) =>
            val newValue =
              headers.get(targetBlockHeader.hash).map(_.vote).getOrElse(BlockHeaderWithVotes(targetBlockHeader))
            val updatedHeaders = headers.updated(targetBlockHeader.hash, newValue)
            votingProcess(updatedPeersToAsk, waitingPeers, pivotBlockNumber, timeout, updatedHeaders)
          case None =>
            blacklist.add(peerId, blacklistDuration, InvalidPivotBlockElectionResponse)
            votingProcess(updatedPeersToAsk, waitingPeers, pivotBlockNumber, timeout, headers)
        }
      case ElectionPivotBlockTimeout =>
        peersToAsk.foreach { peerId =>
          blacklist.add(peerId, blacklistDuration, PivotBlockElectionTimeout)
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

        context become runningPivotBlockElection(
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
      context become runningPivotBlockElection(
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

  private def scheduleRetry(interval: FiniteDuration): Unit = {
    pivotBlockRetryCount = 0
    scheduler.scheduleOnce(interval, self, SelectPivotBlock)
    context become idle
  }

  private def sendResponseAndCleanup(pivotBlockHeader: BlockHeader): Unit = {
    log.info("Found pivot block: {} hash: {}", pivotBlockHeader.number, pivotBlockHeader.hashAsHexString)
    fastSync ! Result(pivotBlockHeader)
    peerEventBus ! Unsubscribe()
    context stop self
  }

  private def obtainBlockHeaderFromPeer(peer: PeerId, blockNumber: BigInt): Unit = {
    peerEventBus ! Subscribe(MessageClassifier(Set(Codes.BlockHeadersCode), PeerSelector.WithId(peer)))
    etcPeerManager ! EtcPeerManagerActor.SendMessage(
      GetBlockHeaders(Left(blockNumber), 1, 0, reverse = false),
      peer
    )
  }

  private def collectVoters(previousBestBlockNumber: Option[BigInt] = None): ElectionDetails = {
    val peersUsedToChooseTarget = peersToDownloadFrom.collect {
      case (_, PeerWithInfo(peer, PeerInfo(_, _, true, maxBlockNumber, _))) =>
        (peer, maxBlockNumber)
    }

    val peersSortedByBestNumber = peersUsedToChooseTarget.toList.sortBy { case (_, number) => -number }
    val bestPeerBestBlockNumber = peersSortedByBestNumber.headOption
      .map { case (_, bestPeerBestBlockNumber) => bestPeerBestBlockNumber }
      .getOrElse(BigInt(0))

    // The current best block number is pushed back by `pivotBlockNumberResetDelta`
    // if this request is issued by the retry logic.
    val currentBestBlockNumber: BigInt =
      previousBestBlockNumber
        .map(_ - pivotBlockNumberResetDelta.max(0))
        .getOrElse(bestPeerBestBlockNumber)

    val expectedPivotBlock = (currentBestBlockNumber - syncConfig.pivotBlockOffset).max(0)
    val correctPeers = peersSortedByBestNumber
      .takeWhile { case (_, number) => number >= expectedPivotBlock }
      .map { case (peer, _) => peer }

    ElectionDetails(correctPeers, currentBestBlockNumber, expectedPivotBlock)
  }
}

object PivotBlockSelector {
  def props(
      etcPeerManager: ActorRef,
      peerEventBus: ActorRef,
      syncConfig: SyncConfig,
      scheduler: Scheduler,
      fastSync: ActorRef,
      blacklist: Blacklist
  ): Props =
    Props(new PivotBlockSelector(etcPeerManager: ActorRef, peerEventBus, syncConfig, scheduler, fastSync, blacklist))

  case object SelectPivotBlock
  final case class Result(targetBlockHeader: BlockHeader)

  case object ElectionPivotBlockTimeout

  case class BlockHeaderWithVotes(header: BlockHeader, votes: Int = 1) {
    def vote: BlockHeaderWithVotes = copy(votes = votes + 1)
  }
  import cats.implicits._
  implicit class SortableHeadersMap(headers: Map[ByteString, BlockHeaderWithVotes]) {
    def mostVotedHeader: Option[BlockHeaderWithVotes] = {
      headers.toList.maximumByOption { case (_, headerWithVotes) => headerWithVotes.votes }.map(_._2)
    }
  }

  final case class ElectionDetails(
      participants: List[Peer],
      currentBestBlockNumber: BigInt,
      expectedPivotBlock: BigInt
  ) {
    def hasEnoughVoters(minNumberOfVoters: Int): Boolean = participants.size >= minNumberOfVoters
  }
}
