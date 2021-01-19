package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler, Terminated}
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.{RequestFailed, ResponseReceived}
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolver._
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, PeerListSupport, PeerRequestHandler}
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class FastSyncBranchResolver(
    val fastSync: ActorRef,
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val blockchain: Blockchain,
    val syncConfig: SyncConfig,
    val appStateStorage: AppStateStorage,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {

  import syncConfig._

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  override def receive: Receive = handleCommonMessages orElse { case StartBranchResolver =>
    getBestPeer match {
      case Some((peer, _)) => requestBlockHeadersGap(peer, blockchain.getBestBlockNumber())
      case None =>
        log.info(s"Still waiting for some peer, rescheduling StartBranchResolver")
        scheduler.scheduleOnce(1.second, self, StartBranchResolver)
    }
  }

  private def waitingBlockHeadersGap(masterPeer: Peer, bestBlockNumber: BigInt): Receive = handleCommonMessages orElse {
    case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) if peer == masterPeer =>
      log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
      getFirstCommonBlock(blockHeaders, bestBlockNumber) match {
        case Some(firstCommonBlockNumber) =>
          finalizeBranchResolver(bestBlockNumber, firstCommonBlockNumber, masterPeer)
        case None =>
          log.info("validation continue with binary searching")
          requestBlockHeader(SearchState(minBlockNumber = 1, maxBlockNumber = bestBlockNumber, masterPeer = masterPeer))
      }
    case ResponseReceived(peer, BlockHeaders(blockHeaders), _) =>
      log.warning(
        "*** Invalid validation - Received {} block headers from a non-master peer {} ***",
        blockHeaders.size,
        peer
      )
    case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
    case Terminated(ref) => log.debug(s"TODO:... $ref") //TODO
  }

  private def binarySearchingLastValidBlock(searchState: SearchState, blockHeaderNumberToSearch: BigInt): Receive =
    handleCommonMessages orElse {
      case ResponseReceived(peer, BlockHeaders(Seq(childBlockHeader: BlockHeader)), timeTaken)
          if peer == searchState.masterPeer && childBlockHeader.number == blockHeaderNumberToSearch =>
        log.info("*** Received 1 block header searched in {} ms ***", timeTaken)
        blockchain
          .getBlockHeaderByNumber(childBlockHeader.number - 1)
          .foreach(validateBlockHeaders(_, childBlockHeader, searchState))
      case ResponseReceived(peer, BlockHeaders(blockHeaders), _) =>
        log.warning("*** Invalid validation - Received {} block headers from peer {} ***", blockHeaders.size, peer)
      case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
      case Terminated(ref) => log.debug(s"TODO:... $ref") //TODO
    }

  private def validateBlockHeaders(
      parentBlockHeader: BlockHeader,
      childBlockHeader: BlockHeader,
      searchState: SearchState
  ): Unit = {
    if (validateBlockHeaders(childBlockHeader, parentBlockHeader)) {
      if (parentBlockHeader.number == searchState.minBlockNumber)
        finalizeBranchResolver(blockchain.getBestBlockNumber(), parentBlockHeader.number, searchState.masterPeer)
      else requestBlockHeader(searchState.copy(minBlockNumber = parentBlockHeader.number + 1))
    } else if (parentBlockHeader.number == searchState.minBlockNumber)
      finalizeBranchResolver(blockchain.getBestBlockNumber(), parentBlockHeader.number - 1, searchState.masterPeer)
    else
      requestBlockHeader(searchState.copy(maxBlockNumber = parentBlockHeader.number))
  }

  private def finalizeBranchResolver(lastBlock: BigInt, firstCommonBlockNumber: BigInt, masterPeer: Peer): Unit = {
    discardLastInvalidBlocks(lastBlock, firstCommonBlockNumber)
    context become receive
    log.info(s"branch resolution completed - firstCommonBlockNumber $firstCommonBlockNumber")
    fastSync ! BranchResolvedSuccessful(firstCommonBlockNumber = firstCommonBlockNumber, masterPeer = masterPeer)
  }

  private def getBestPeer: Option[(Peer, PeerInfo)] = {
    handshakedPeers.toList.sortBy { case (_, peerInfo) => peerInfo.maxBlockNumber }(Ordering[BigInt].reverse).headOption
  }

  private def requestBlockHeadersGap(masterPeer: Peer, bestBlockNumber: BigInt): Unit = {
    getBlockHeaders(masterPeer, bestBlockNumber - blockHeadersPerRequest + 1, blockHeadersPerRequest)
    context become waitingBlockHeadersGap(masterPeer, bestBlockNumber)
  }

  private def requestBlockHeader(searchState: SearchState): Unit = {
    val blockHeaderNumber: BigInt =
      searchState.minBlockNumber + (searchState.maxBlockNumber - searchState.minBlockNumber) / 2
    val blockHeaderNumberToSearch: BigInt = blockHeaderNumber + 1
    getBlockHeaders(searchState.masterPeer, blockHeaderNumberToSearch, 1)
    context become binarySearchingLastValidBlock(searchState, blockHeaderNumberToSearch)
  }

  private def getBlockHeaders(peer: Peer, blockNumber: BigInt, maxHeaders: BigInt): Unit = {
    val handler = context.actorOf(
      PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
        peer,
        peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetBlockHeaders(Left(blockNumber), maxHeaders, skip = 0, reverse = false),
        responseMsgCode = Codes.BlockHeadersCode
      )
    )

    context watch handler
  }

  private def getFirstCommonBlock(candidateHeaders: Seq[BlockHeader], bestBlockNumber: BigInt): Option[BigInt] = {
    candidateHeaders.reverse
      .zip(bestBlockNumber to ((bestBlockNumber - blockHeadersPerRequest) max 1) by -1)
      .find { case (child, childNumber) =>
        blockchain.getBlockHeaderByNumber(childNumber).exists { parent => validateBlockHeaders(child, parent) }
      }
      .map(_._2)
  }

  private def validateBlockHeaders(childBlockHeader: BlockHeader, parentBlockHeader: BlockHeader): Boolean =
    parentBlockHeader.isParentOf(childBlockHeader)

  private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String): Unit = {
    log.error(s"response failure from peer $peer - reason $reason")
    context unwatch handler
    if (handshakedPeers.contains(peer)) {
      blacklist(peer.id, blacklistDuration, reason) //Only in blacklist??
    }
    self ! StartBranchResolver
  }

  private def discardLastInvalidBlocks(lastBlock: BigInt, lastValidBlock: BigInt): Unit = {
    (lastBlock to (lastValidBlock + 1) by -1).foreach { n =>
      blockchain.getBlockHeaderByNumber(n).foreach { headerToRemove =>
        blockchain.removeBlock(headerToRemove.hash, withState = false)
      }
    }
    // TODO (maybe ETCM-77): Manage last checkpoint number too
    appStateStorage.putBestBlockNumber((lastValidBlock - 1) max 0).commit()
  }
}

object FastSyncBranchResolver {
  def prop(
      fastSync: ActorRef,
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      appStateStorage: AppStateStorage,
      scheduler: Scheduler
  ): Props =
    Props(
      new FastSyncBranchResolver(
        fastSync,
        peerEventBus,
        etcPeerManager,
        blockchain,
        syncConfig,
        appStateStorage,
        scheduler
      )
    )

  case class SearchState(minBlockNumber: BigInt, maxBlockNumber: BigInt, masterPeer: Peer)

  sealed trait BranchResolverRequest
  case object StartBranchResolver extends BranchResolverRequest

  sealed trait BranchResolverResponse
  case class BranchResolvedSuccessful(firstCommonBlockNumber: BigInt, masterPeer: Peer) extends BranchResolverResponse
}
