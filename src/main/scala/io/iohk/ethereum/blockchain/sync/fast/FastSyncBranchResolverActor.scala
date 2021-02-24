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
    with RecentBlocksSearchSupport
    with BinarySearchSupport
    with PeerListSupportNg
    with Timers {

  import FastSyncBranchResolverActor._

  override protected val recentHeadersSize: Int = syncConfig.blockHeadersPerRequest

  override def receive: Receive = waitingForPeerWithHighestBlock

  private def waitingForPeerWithHighestBlock: Receive = handlePeerListMessages orElse { case StartBranchResolver =>
    getPeerWithHighestBlock match {
      case Some(PeerWithInfo(peer, _)) => requestRecentBlockHeaders(peer, blockchain.getBestBlockNumber())
      case None =>
        log.info(s"Waiting for peers, rescheduling StartBranchResolver")
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
          verifyRecentBlockHeadersResponse(headers, masterPeer, bestBlockNumber)
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
        headers match {
          case childHeader :: Nil if childHeader.number == blockHeaderNumberToSearch =>
            log.debug(s"Received requested block header from peer [${peer.id}] in $durationMs ms")
            blockchain.getBlockHeaderByNumber(parentOf(childHeader.number)) match {
              case Some(parentHeader) => validateBlockHeaders(parentHeader, childHeader, searchState)
              case None => stopWithFailure(BranchResolutionFailed.blockHeaderNotFound(blockHeaderNumberToSearch))
            }
          case _ => handleInvalidResponse(peer, requestHandler)
        }
      case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
      case Terminated(ref) if ref == requestHandler => handlePeerTermination(searchState.masterPeer, ref)
    }
  }

  private def requestRecentBlockHeaders(masterPeer: Peer, bestBlockNumber: BigInt): Unit = {
    val requestHandler = sendGetBlockHeadersRequest(
      masterPeer,
      fromBlock = childOf(bestBlockNumber - recentHeadersSize),
      amount = recentHeadersSize
    )
    context.become(waitingForRecentBlockHeaders(masterPeer, bestBlockNumber, requestHandler))
  }

  /**
    * Searches recent blocks for a valid parent/child relationship.
    * If we dont't find one, we switch to binary search.
    */
  private def verifyRecentBlockHeadersResponse(
      blockHeaders: Seq[BlockHeader],
      masterPeer: Peer,
      bestBlockNumber: BigInt
  ): Unit = {
    getFirstCommonBlock(blockHeaders, bestBlockNumber) match {
      case Some(firstCommonBlockNumber) =>
        finalizeBranchResolver(firstCommonBlockNumber, masterPeer)
      case None =>
        log.info(SwitchToBinarySearchLog, recentHeadersSize)
        requestBlockHeader(
          SearchState(minBlockNumber = 0, maxBlockNumber = bestBlockNumber, masterPeer)
        )
    }
  }

  private def requestBlockHeader(searchState: SearchState): Unit = {
    val middleBlockHeaderNumber: BigInt =
      searchState.minBlockNumber + (searchState.maxBlockNumber - searchState.minBlockNumber) / 2
    val blockHeaderNumberToRequest: BigInt = childOf(middleBlockHeaderNumber)
    val handler = sendGetBlockHeadersRequest(searchState.masterPeer, blockHeaderNumberToRequest, 1)
    context.become(waitingForBinarySearchBlock(searchState, blockHeaderNumberToRequest, handler))
  }

  private def finalizeBranchResolver(firstCommonBlockNumber: BigInt, masterPeer: Peer): Unit = {
    discardBlocksAfter(firstCommonBlockNumber)
    log.info(s"Branch resolution completed - firstCommonBlockNumber [$firstCommonBlockNumber]")
    fastSync ! BranchResolvedSuccessful(firstCommonBlockNumber = firstCommonBlockNumber, masterPeer = masterPeer)
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
      syncConfig.blacklistDuration,
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
    log.warning(
      s"Peer request handler [${peerHandlerRef.path.name}] for peer [${peer.id}] terminated. Restarting branch resolver."
    )
    restart()
  }

  // TODO track restarts and give up after a couple of times
  private def restart(): Unit = {
    context.become(waitingForPeerWithHighestBlock)
    self ! StartBranchResolver
  }

}

object FastSyncBranchResolverActor {

  protected val RestartTimerKey: String = "Restart"

  protected val InvalidResponseLog: String =
    "*** Invalid response - Received {} block headers from peer {}. Requested {} headers. Current master peer {}. ***"

  protected val SwitchToBinarySearchLog: String =
    "Branch diverged earlier than {} blocks ago. Switching to binary search to determine first common block."

  def prop(
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

  /**
    * Stores the current search state for binary search.
    * Meaning we know the first common block lies between minBlockNumber and maxBlockNumber.
    */
  final case class SearchState(minBlockNumber: BigInt, maxBlockNumber: BigInt, masterPeer: Peer)

  sealed trait BranchResolverRequest
  case object StartBranchResolver extends BranchResolverRequest

  sealed trait BranchResolverResponse
  final case class BranchResolvedSuccessful(firstCommonBlockNumber: BigInt, masterPeer: Peer)
      extends BranchResolverResponse
  import BranchResolutionFailed._
  final case class BranchResolutionFailed(failure: BranchResolutionFailure)
  object BranchResolutionFailed {
    def noCommonBlock: BranchResolutionFailed = BranchResolutionFailed(NoCommonBlockFound)
    def tooManyRestarts(numRestarts: Int): BranchResolutionFailed = BranchResolutionFailed(TooManyRestarts(numRestarts))
    def blockHeaderNotFound(blockHeaderNum: BigInt): BranchResolutionFailed = BranchResolutionFailed(
      BlockHeaderNotFound(blockHeaderNum)
    )

    sealed trait BranchResolutionFailure
    final case object NoCommonBlockFound extends BranchResolutionFailure
    final case class TooManyRestarts(numRestarts: Int) extends BranchResolutionFailure
    final case class BlockHeaderNotFound(blockHeaderNum: BigInt) extends BranchResolutionFailure
  }

}
