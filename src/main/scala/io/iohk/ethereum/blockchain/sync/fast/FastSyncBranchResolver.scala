package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
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

  private def waitingBlockHeadersGap(bestBlockNumber: BigInt): Receive = handleCommonMessages orElse {
    case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) => {
      log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
      getLastCorrectBlock(blockHeaders, bestBlockNumber) match {
        case Some(lastValidBlockHeader) =>
          finalizeBranchResolver(bestBlockNumber, lastValidBlockHeader)
        case None => requestBlockHeader(peer, SearchState(0, bestBlockNumber))
      }
    }
    case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
  }

  private def binarySearchingLastValidBlock(searchState: SearchState): Receive = handleCommonMessages orElse {
    case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
      log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
      for {
        parentBlockHeader <- blockHeaders.headOption
        childBlockHeader <- blockchain.getBlockHeaderByNumber(parentBlockHeader.number - 1)
      } yield validateBlockHeaders(parentBlockHeader, childBlockHeader, searchState, peer)

    case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
  }

  private def validateBlockHeaders(
      parentBlockHeader: BlockHeader,
      childBlockHeader: BlockHeader,
      searchState: SearchState,
      peer: Peer
  ): Unit = {
    if (validateBlockHeaders(parentBlockHeader, childBlockHeader)) {
      if (childBlockHeader.number == searchState.minBlockNumber)
        finalizeBranchResolver(blockchain.getBestBlockNumber(), childBlockHeader.number)
      else requestBlockHeader(peer, searchState.copy(minBlockNumber = childBlockHeader.number + 1))
    } else requestBlockHeader(peer, searchState.copy(maxBlockNumber = childBlockHeader.number))
  }

  private def finalizeBranchResolver(lastBlock: BigInt, lastValidBlock: BigInt): Unit = {
    discardLastInvalidBlocks(lastBlock, lastValidBlock)
    context become receive
    log.info("branch resolution completed")
    fastSync ! BranchResolvedSuccessful
  }

  private def getBestPeer: Option[(Peer, PeerInfo)] = {
    handshakedPeers.toList.sortBy { case (_, peerInfo) => peerInfo.maxBlockNumber }(Ordering[BigInt].reverse).headOption
  }

  private def requestBlockHeadersGap(peer: Peer, bestBlockNumber: BigInt): Unit = {
    getBlockHeaders(peer, bestBlockNumber - blockHeadersPerRequest + 1, blockHeadersPerRequest)
    context become waitingBlockHeadersGap(bestBlockNumber)
  }

  private def requestBlockHeader(peer: Peer, searchState: SearchState): Unit = {
    val blockHeaderNumber: BigInt =
      searchState.minBlockNumber + (searchState.maxBlockNumber - searchState.minBlockNumber) / 2
    getBlockHeaders(peer, blockHeaderNumber + 1, 1)
    context become binarySearchingLastValidBlock(searchState)
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
      ),
      BlockHeadersHandlerName
    )

    context watch handler
  }

  private def getLastCorrectBlock(candidateHeaders: Seq[BlockHeader], bestBlockNumber: BigInt): Option[BigInt] = {
    candidateHeaders.reverse
      .zip(bestBlockNumber to ((bestBlockNumber - blockHeadersPerRequest) max 1) by -1)
      .find { case (child, childNumber) =>
        blockchain.getBlockHeaderByNumber(childNumber).exists { parent => validateBlockHeaders(child, parent) }
      }
      .map(_._2)
  }

  private def validateBlockHeaders(childBlockHeader: BlockHeader, parentBlockHeader: BlockHeader): Boolean =
    parentBlockHeader.hash == childBlockHeader.parentHash && parentBlockHeader.number + 1 == childBlockHeader.number

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

  private val BlockHeadersHandlerName = "block-headers-request-handler"

  case class SearchState(minBlockNumber: BigInt, maxBlockNumber: BigInt)

  sealed trait BranchResolverRequest
  object StartBranchResolver extends BranchResolverRequest

  sealed trait BranchResolverResponse
  object BranchResolvedSuccessful extends BranchResolverResponse
}
