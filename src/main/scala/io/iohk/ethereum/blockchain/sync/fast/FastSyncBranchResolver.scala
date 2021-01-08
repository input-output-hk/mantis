package io.iohk.ethereum.blockchain.sync.fast

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler}
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler
import io.iohk.ethereum.blockchain.sync.fast.FastSync.PeerWithInfo
import io.iohk.ethereum.blockchain.sync.fast.FastSyncBranchResolver._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.p2p.messages.Codes
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.utils.Config.SyncConfig

class FastSyncBranchResolver(
                              val peers: List[PeerWithInfo],
                              val syncState: SyncState,
                              val peerEventBus: ActorRef,
                              val etcPeerManager: ActorRef,
                              val syncConfig: SyncConfig,
                              implicit val scheduler: Scheduler
                            ) extends Actor
  with ActorLogging {

  import syncConfig._


  override def receive: Receive = ???



  private def getBestPeer(peers: List[PeerWithInfo]){
    val sdf = peers.sortBy(_.info.maxBlockNumber)(Ordering[BigInt].reverse)
    sdf
  }

  def requestBlockHeaders(peer: Peer): Unit = {
    val limit: BigInt = ???
     /* if (blockHeadersPerRequest < (syncState.safeDownloadTarget - syncState.bestBlockHeaderNumber))
        blockHeadersPerRequest
      else
        syncState.safeDownloadTarget - syncState.bestBlockHeaderNumber*/

    val handler = context.actorOf(
      PeerRequestHandler.props[GetBlockHeaders, BlockHeaders](
        peer,
        peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetBlockHeaders(Left(syncState.bestBlockHeaderNumber + 1), limit, skip = 0, reverse = false),
        responseMsgCode = Codes.BlockHeadersCode
      ),
      BlockHeadersHandlerName
    )

    context watch handler
    //assignedHandlers += (handler -> peer)
    //requestedHeaders += (peer -> limit)
    //peerRequestsTime += (peer -> Instant.now())
  }

  def getLastCorrectBlock(candidateHeaders: Seq[BlockHeader], currentHeaders: Seq[BlockHeader]): Option[BlockHeader] = {
    candidateHeaders.zip(currentHeaders).find { case (parent, child) => {
      parent.hash == child.parentHash && parent.number + 1 == child.number
    }}.map(_._2)
  }
}

object FastSyncBranchResolver {
  def prop(peers: List[PeerWithInfo]): Props = Props(new FastSyncBranchResolver(peers))

  private val BlockHeadersHandlerName = "block-headers-request-handler"
  case class SyncState(
                        bestBlockHeaderNumber: BigInt = 0,
                      )
}