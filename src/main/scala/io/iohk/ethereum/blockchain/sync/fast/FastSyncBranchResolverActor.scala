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
    with PeerListSupportNg
    with Timers {

  private val recentHeadersSize: Int = syncConfig.blockHeadersPerRequest // TODO config name is not very descriptive

  override def receive: Receive = waitingForPeerWithHighestBlock

  private def waitingForPeerWithHighestBlock: Receive = handlePeerListMessages orElse { case StartBranchResolver =>
    getPeerWithHighestBlock match {
      case Some(PeerWithInfo(peer, _)) => requestRecentBlockHeaders(peer, blockchain.getBestBlockNumber())
      case None =>
        log.info(s"Waiting for a peer, rescheduling StartBranchResolver")
        timers.startSingleTimer(RestartTimerKey, StartBranchResolver, 1.second)
    }
  }

  // FIXME if bestBlockNumber changed while making the request, there will be a mismatch
  private def waitingForRecentBlockHeaders(masterPeer: Peer, bestBlockNumber: BigInt): Receive =
    handlePeerListMessages orElse {
      case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken)
          if peer == masterPeer && blockHeaders.size == recentHeadersSize =>
        log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
        verifyRecentBlockHeadersResponse(blockHeaders, masterPeer, bestBlockNumber)
      case ResponseReceived(peer, BlockHeaders(blockHeaders), _) =>
        log.warning(InvalidResponseLog, blockHeaders.size, peer, recentHeadersSize, masterPeer)
      case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
      case Terminated(ref) => log.debug(s"TODO:... $ref") //TODO
    }

  private def verifyRecentBlockHeadersResponse(
      blockHeaders: Seq[BlockHeader],
      masterPeer: Peer,
      bestBlockNumber: BigInt
  ): Unit = {
    getFirstCommonBlock(blockHeaders, bestBlockNumber) match {
      case Some(firstCommonBlockNumber) =>
        finalizeBranchResolver(bestBlockNumber, firstCommonBlockNumber, masterPeer)
      case None =>
        log.info(SwitchToBinarySearchLog, recentHeadersSize)
        requestBlockHeader(SearchState(minBlockNumber = 1, maxBlockNumber = bestBlockNumber, masterPeer))
    }
  }

  private def binarySearchingLastValidBlock(searchState: SearchState, blockHeaderNumberToSearch: BigInt): Receive =
    handlePeerListMessages orElse {
      case ResponseReceived(peer, BlockHeaders(Seq(childBlockHeader: BlockHeader)), timeTaken)
          if peer == searchState.masterPeer && childBlockHeader.number == blockHeaderNumberToSearch =>
        log.info("*** Received 1 block header searched in {} ms ***", timeTaken)
        blockchain
          .getBlockHeaderByNumber(childBlockHeader.number - 1)
          .foreach(validateBlockHeaders(_, childBlockHeader, searchState))
      case ResponseReceived(peer, BlockHeaders(blockHeaders), _) =>
        log.warning("*** Invalid response - Received {} block headers from peer {} ***", blockHeaders.size, peer)
      case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
      case Terminated(ref) => log.debug(s"TODO:... $ref") //TODO
    }

  private def validateBlockHeaders(
      parentBlockHeader: BlockHeader,
      childBlockHeader: BlockHeader,
      searchState: SearchState
  ): Unit = {
    if (parentBlockHeader.isParentOf(childBlockHeader)) {
      if (parentBlockHeader.number == searchState.minBlockNumber)
        finalizeBranchResolver(blockchain.getBestBlockNumber(), parentBlockHeader.number, searchState.masterPeer)
      else requestBlockHeader(searchState.copy(minBlockNumber = childOf(parentBlockHeader.number)))
    } else if (parentBlockHeader.number == searchState.minBlockNumber)
      finalizeBranchResolver(
        lastBlock = blockchain.getBestBlockNumber(),
        firstCommonBlockNumber = parentOf(parentBlockHeader.number),
        masterPeer = searchState.masterPeer
      )
    else
      requestBlockHeader(searchState.copy(maxBlockNumber = parentBlockHeader.number))
  }

  private def finalizeBranchResolver(lastBlock: BigInt, firstCommonBlockNumber: BigInt, masterPeer: Peer): Unit = {
    discardBlocksAfter(firstCommonBlockNumber)
    log.info(s"branch resolution completed - firstCommonBlockNumber $firstCommonBlockNumber")
    fastSync ! BranchResolvedSuccessful(firstCommonBlockNumber = firstCommonBlockNumber, masterPeer = masterPeer)
    context.self ! PoisonPill
  }

  private def requestRecentBlockHeaders(masterPeer: Peer, bestBlockNumber: BigInt): Unit = {
    sendGetBlockHeadersRequest(
      masterPeer,
      childOf(bestBlockNumber - recentHeadersSize),
      recentHeadersSize
    )
    context.become(waitingForRecentBlockHeaders(masterPeer, bestBlockNumber))
  }

  private def requestBlockHeader(searchState: SearchState): Unit = {
    val blockHeaderNumber: BigInt =
      searchState.minBlockNumber + (searchState.maxBlockNumber - searchState.minBlockNumber) / 2
    val blockHeaderNumberToSearch: BigInt = childOf(blockHeaderNumber)
    sendGetBlockHeadersRequest(searchState.masterPeer, blockHeaderNumberToSearch, 1)
    context become binarySearchingLastValidBlock(searchState, blockHeaderNumberToSearch)
  }

  private def sendGetBlockHeadersRequest(peer: Peer, blockNumber: BigInt, maxHeaders: BigInt): Unit = {
    val handler = context.actorOf(
      PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
        peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetBlockHeaders(Left(blockNumber), maxHeaders, skip = 0, reverse = false),
        responseMsgCode = Codes.BlockHeadersCode
      )
    )
    context.watch(handler)
  }

  private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String): Unit = {
    log.error(s"Response failure from peer $peer - reason $reason")
    context unwatch handler
    blacklistIfHandshaked(
      peer.id,
      syncConfig.blacklistDuration,
      BlacklistReason.RequestFailed(reason)
    ) // TODO comment by max: Only in blacklist??
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
    * Stores the current search state, that means we know the first common block lies
    * between minBlockNumber and maxBlockNumber.
    */
  final case class SearchState(minBlockNumber: BigInt, maxBlockNumber: BigInt, masterPeer: Peer)

  sealed trait BranchResolverRequest
  case object StartBranchResolver extends BranchResolverRequest

  sealed trait BranchResolverResponse
  final case class BranchResolvedSuccessful(firstCommonBlockNumber: BigInt, masterPeer: Peer)
      extends BranchResolverResponse
}
