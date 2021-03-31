package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Scheduler, Terminated}
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistReason
import io.iohk.ethereum.blockchain.sync.PeerListSupportNg.PeerWithInfo
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.{RequestFailed, ResponseReceived}
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolverActor._
import io.iohk.ethereum.blockchain.sync.{Blacklist, PeerListSupportNg, PeerRequestHandler}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import monix.eval.Coeval
import monix.catnap.cancelables.AssignableCancelableF.Bool
import akka.actor.Timers
import io.iohk.ethereum.blockchain.sync.fast.BinarySearchSupport.ContinueBinarySearch

class FastSyncBranchResolverActor(
    val fastSync: ActorRef,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val blockchain: Blockchain,
    val blacklist: Blacklist,
    val syncConfig: SyncConfig,
    val appStateStorage: AppStateStorage,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with FastSyncBranchResolver
    with PeerListSupportNg
    with Timers {

  import FastSyncBranchResolverActor._
  import FastSyncBranchResolver._
  import BinarySearchSupport._

  private val recentHeadersSize: Int = syncConfig.blockHeadersPerRequest

  private val recentBlocksSearch: RecentBlocksSearch = new RecentBlocksSearch(blockchain)

  override def receive: Receive = waitingForPeerWithHighestBlock

  private def waitingForPeerWithHighestBlock: Receive = handlePeerListMessages orElse { case StartBranchResolver =>
    getPeerWithHighestBlock match {
      case Some(PeerWithInfo(peer, _)) => requestRecentBlockHeaders(peer, blockchain.getBestBlockNumber())
      case None =>
        log.info("Waiting for peers, rescheduling StartBranchResolver")
        timers.startSingleTimer(RestartTimerKey, StartBranchResolver, 1.second)
    }
  }

  private def waitingForRecentBlockHeaders(
      masterPeer: Peer,
      bestBlockNumber: BigInt,
      requestHandler: ActorRef
  ): Receive =
    handlePeerListMessages orElse {
      case ResponseReceived(peer, BlockHeaders(headers), timeTaken) if peer == masterPeer =>
        if (headers.size == recentHeadersSize) {
          log.debug("Received {} block headers from peer {} in {} ms", headers.size, masterPeer.id, timeTaken)
          handleRecentBlockHeadersResponse(headers, masterPeer, bestBlockNumber)
        } else {
          handleInvalidResponse(peer, requestHandler)
        }
      case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
      case Terminated(ref) if ref == requestHandler => handlePeerTermination(masterPeer, ref)
    }

  private def waitingForBinarySearchBlock(
      searchState: SearchState,
      blockHeaderNumberToSearch: BigInt,
      requestHandler: ActorRef
  ): Receive = {
    handlePeerListMessages orElse {
      case ResponseReceived(peer, BlockHeaders(headers), durationMs) if peer == searchState.masterPeer =>
        context.unwatch(requestHandler)
        headers.toList match {
          case childHeader :: Nil if childHeader.number == blockHeaderNumberToSearch =>
            log.debug(ReceivedBlockHeaderLog, blockHeaderNumberToSearch, peer.id, durationMs)
            handleBinarySearchBlockHeaderResponse(searchState, childHeader)
          case _ =>
            log.warning(ReceivedWrongHeaders, blockHeaderNumberToSearch, headers.map(_.number))
            handleInvalidResponse(peer, requestHandler)
        }
      case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
      case Terminated(ref) if ref == requestHandler => handlePeerTermination(searchState.masterPeer, ref)
      case Terminated(_) => () // ignore
    }
  }

  private def requestRecentBlockHeaders(masterPeer: Peer, bestBlockNumber: BigInt): Unit = {
    val requestHandler = sendGetBlockHeadersRequest(
      masterPeer,
      fromBlock = childOf((bestBlockNumber - recentHeadersSize).max(0)),
      amount = recentHeadersSize
    )
    context.become(waitingForRecentBlockHeaders(masterPeer, bestBlockNumber, requestHandler))
  }

  /**
    * Searches recent blocks for a valid parent/child relationship.
    * If we dont't find one, we switch to binary search.
    */
  private def handleRecentBlockHeadersResponse(
      blockHeaders: Seq[BlockHeader],
      masterPeer: Peer,
      bestBlockNumber: BigInt
  ): Unit = {
    recentBlocksSearch.getHighestCommonBlock(blockHeaders, bestBlockNumber) match {
      case Some(highestCommonBlockNumber) =>
        finalizeBranchResolver(highestCommonBlockNumber, masterPeer)
      case None =>
        log.info(SwitchToBinarySearchLog, recentHeadersSize)
        requestBlockHeaderForBinarySearch(
          SearchState(minBlockNumber = 1, maxBlockNumber = bestBlockNumber, masterPeer)
        )
    }
  }

  private def requestBlockHeaderForBinarySearch(searchState: SearchState): Unit = {
    val headerNumberToRequest = blockHeaderNumberToRequest(searchState.minBlockNumber, searchState.maxBlockNumber)
    val handler = sendGetBlockHeadersRequest(searchState.masterPeer, headerNumberToRequest, 1)
    context.become(waitingForBinarySearchBlock(searchState, headerNumberToRequest, handler))
  }

  private def handleBinarySearchBlockHeaderResponse(searchState: SearchState, childHeader: BlockHeader): Unit = {
    import BinarySearchSupport._
    blockchain.getBlockHeaderByNumber(parentOf(childHeader.number)) match {
      case Some(parentHeader) =>
        validateBlockHeaders(parentHeader, childHeader, searchState) match {
          case NoCommonBlock => stopWithFailure(BranchResolutionFailed.noCommonBlock)
          case BinarySearchCompleted(highestCommonBlockNumber) =>
            finalizeBranchResolver(highestCommonBlockNumber, searchState.masterPeer)
          case ContinueBinarySearch(newSearchState) =>
            log.debug(s"Continuing binary search with new search state: $newSearchState")
            requestBlockHeaderForBinarySearch(newSearchState)
        }
      case None => stopWithFailure(BranchResolutionFailed.blockHeaderNotFound(childHeader.number))
    }
  }

  private def finalizeBranchResolver(firstCommonBlockNumber: BigInt, masterPeer: Peer): Unit = {
    discardBlocksAfter(firstCommonBlockNumber)
    log.info(s"Branch resolution completed with first common block number [$firstCommonBlockNumber]")
    fastSync ! BranchResolvedSuccessful(highestCommonBlockNumber = firstCommonBlockNumber, masterPeer = masterPeer)
    context.stop(self)
  }

  /**
    * In case of fatal errors (and to prevent trying forever) branch resolver will signal fast-sync about
    * the error and let fast-sync decide if it issues another request.
    */
  private def stopWithFailure(response: BranchResolutionFailed): Unit = {
    fastSync ! response
    context.stop(self)
  }

  private def sendGetBlockHeadersRequest(peer: Peer, fromBlock: BigInt, amount: BigInt): ActorRef = {
    val handler = context.actorOf(
      PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
        peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetBlockHeaders(Left(fromBlock), amount, skip = 0, reverse = false),
        responseMsgCode = Codes.BlockHeadersCode
      )
    )
    context.watch(handler)
    handler
  }

  private def handleInvalidResponse(peer: Peer, peerRef: ActorRef): Unit = {
    log.warning(s"Received invalid response from peer [${peer.id}]. Restarting branch resolver.")
    context.unwatch(peerRef)
    blacklistIfHandshaked(
      peer.id,
      syncConfig.criticalBlacklistDuration,
      BlacklistReason.WrongBlockHeaders
    )
    restart()
  }

  private def handleRequestFailure(peer: Peer, peerRef: ActorRef, reason: String): Unit = {
    log.warning(s"Request to peer [${peer.id}] failed: [$reason]. Restarting branch resolver.")
    context.unwatch(peerRef)
    blacklistIfHandshaked(
      peer.id,
      syncConfig.blacklistDuration,
      BlacklistReason.RequestFailed(reason)
    )
    restart()
  }

  private def handlePeerTermination(peer: Peer, peerHandlerRef: ActorRef): Unit = {
    log.warning(peerTerminatedLog, peerHandlerRef.path.name, peer.id)
    restart()
  }

  private def restart(): Unit = {
    context.become(waitingForPeerWithHighestBlock)
    self ! StartBranchResolver
  }

}

object FastSyncBranchResolverActor {

  protected val RestartTimerKey: String = "Restart"

  protected val InvalidHeadersResponseLog: String =
    "Invalid response - Received {} block headers from peer {}. Requested {} headers. Current master peer {}"

  protected val SwitchToBinarySearchLog: String =
    "Branch diverged earlier than {} blocks ago. Switching to binary search to determine first common block."

  protected val ReceivedBlockHeaderLog: String =
    "Received requested block header [{}] from peer [{}] in {} ms"

  protected val ReceivedWrongHeaders: String =
    "Received invalid response when requesting block header [{}]. Received: {}"

  protected val peerTerminatedLog: String =
    "Peer request handler [{}] for peer [{}] terminated. Restarting branch resolver."

  def props(
      fastSync: ActorRef,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      blockchain: Blockchain,
      blacklist: Blacklist,
      syncConfig: SyncConfig,
      appStateStorage: AppStateStorage,
      scheduler: Scheduler
  ): Props =
    Props(
      new FastSyncBranchResolverActor(
        fastSync,
        peerEventBus,
        etcPeerManager,
        blockchain,
        blacklist,
        syncConfig,
        appStateStorage,
        scheduler
      )
    )

  sealed trait BranchResolverRequest
  case object StartBranchResolver extends BranchResolverRequest

  sealed trait BranchResolverResponse
  final case class BranchResolvedSuccessful(highestCommonBlockNumber: BigInt, masterPeer: Peer)
      extends BranchResolverResponse
  import BranchResolutionFailed._
  final case class BranchResolutionFailed(failure: BranchResolutionFailure)
  object BranchResolutionFailed {
    def noCommonBlock: BranchResolutionFailed = BranchResolutionFailed(NoCommonBlockFound)
    def blockHeaderNotFound(blockHeaderNum: BigInt): BranchResolutionFailed = BranchResolutionFailed(
      BlockHeaderNotFound(blockHeaderNum)
    )

    sealed trait BranchResolutionFailure
    final case object NoCommonBlockFound extends BranchResolutionFailure
    final case class BlockHeaderNotFound(blockHeaderNum: BigInt) extends BranchResolutionFailure
  }

}
