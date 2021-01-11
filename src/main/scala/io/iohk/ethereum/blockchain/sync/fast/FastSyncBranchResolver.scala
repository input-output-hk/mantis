package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.{RequestFailed, ResponseReceived}
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolver._
import io.iohk.ethereum.blockchain.sync.{BlacklistSupport, PeerListSupport, PeerRequestHandler}
import io.iohk.ethereum.domain.{BlockHeader, Blockchain}
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.SyncConfig

class FastSyncBranchResolver(
    val peerEventBus: ActorRef,
    val etcPeerManager: ActorRef,
    val blockchain: Blockchain,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with PeerListSupport
    with BlacklistSupport {

  import syncConfig._

  val initiator: ActorRef = context.parent

  override def receive: Receive = { case StartBranchResolver() =>
    getBestPeer match {
      case Some((peer, _)) => requestBlockHeaders(peer, blockchain.getBestBlockNumber())
      case None => ???
    }
  }

  private def waitingForLastBlocks(bestBlockNumber: BigInt): Receive = {
    case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
      log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
      getLastCorrectBlock(blockHeaders, bestBlockNumber) match {
        case Some(lastValidBlockHeader) => initiator ! LastValidBlock(lastValidBlockHeader)
        case None => requestBlockHeader(peer, SearchState(0, bestBlockNumber))
      }
    case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
  }

  private def searchingLastValidBlock(searchState: SearchState): Receive = {
    case ResponseReceived(peer, BlockHeaders(blockHeaders), timeTaken) =>
      log.info("*** Received {} block headers in {} ms ***", blockHeaders.size, timeTaken)
      for {
        parentBlockHeader <- blockHeaders.headOption
        childBlockHeader <- blockchain.getBlockHeaderByNumber(parentBlockHeader.number - 1)
      } yield validateBlocks(parentBlockHeader, childBlockHeader, searchState, peer)

    case RequestFailed(peer, reason) => handleRequestFailure(peer, sender(), reason)
  }

  private def validateBlocks(parentBlockHeader: BlockHeader, childBlockHeader: BlockHeader, searchState: SearchState, peer: Peer): Unit ={
    if (validateBlockHeaders(parentBlockHeader, childBlockHeader)){
      if(childBlockHeader.number == searchState.minBlockNumber)
        initiator ! LastValidBlock(childBlockHeader.number)
      else requestBlockHeader(peer, searchState.copy(minBlockNumber = childBlockHeader.number + 1))
    }
    else requestBlockHeader(peer, searchState.copy(maxBlockNumber = childBlockHeader.number))
  }

  private def getBestPeer: Option[(Peer, PeerInfo)] = {
    handshakedPeers.toList.sortBy { case (_, peerInfo) => peerInfo.maxBlockNumber }(Ordering[BigInt].reverse).headOption
  }

  private def requestBlockHeaders(peer: Peer, bestBlockNumber: BigInt): Unit = {
    getBlockHeaders(peer, bestBlockNumber - blockHeadersPerRequest + 1, blockHeadersPerRequest)
    context become waitingForLastBlocks(bestBlockNumber)
  }

  private def requestBlockHeader(peer: Peer, searchState: SearchState): Unit = {
    val blockHeaderNumber: BigInt = searchState.minBlockNumber + (searchState.maxBlockNumber - searchState.minBlockNumber) / 2
    getBlockHeaders(peer, blockHeaderNumber + 1, 1)
    context become searchingLastValidBlock(searchState)
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

  private def getLastCorrectBlock(candidateHeaders: Seq[BlockHeader], bestBlock: BigInt): Option[BigInt] = {
    candidateHeaders
      .zip(bestBlock to ((bestBlock - blockHeadersPerRequest) max 1) by -1)
      .find { case (parent, childNumber) =>
        blockchain.getBlockHeaderByNumber(childNumber).exists { child => validateBlockHeaders(parent, child) }
      }
      .map(_._2)
  }

  private def validateBlockHeaders(parentBlockHeader: BlockHeader, childBlockHeader: BlockHeader): Boolean =
    parentBlockHeader.hash == childBlockHeader.parentHash && parentBlockHeader.number + 1 == childBlockHeader.number

  private def handleRequestFailure(peer: Peer, handler: ActorRef, reason: String): Unit = {
    log.error(s"response failure from peer $peer - reason $reason")
    context unwatch handler
    if (handshakedPeers.contains(peer)) {
      blacklist(peer.id, blacklistDuration, reason) //Only in blacklist??
    }
    self ! StartBranchResolver()
  }
}

object FastSyncBranchResolver {
  def prop(
      peerEventBus: ActorRef,
      etcPeerManager: ActorRef,
      blockchain: Blockchain,
      syncConfig: SyncConfig,
      scheduler: Scheduler
  ): Props =
    Props(new FastSyncBranchResolver(peerEventBus, etcPeerManager, blockchain, syncConfig, scheduler))

  private val BlockHeadersHandlerName = "block-headers-request-handler"

  case class SearchState(minBlockNumber: BigInt, maxBlockNumber: BigInt)

  sealed trait BranchResolver
  case class StartBranchResolver() extends BranchResolver


  case class LastValidBlock(blockNumber: BigInt)
}
