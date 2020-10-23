package io.iohk.ethereum.blockchain.sync

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Scheduler, Timers}
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.PeerRequestHandler.ResponseReceived
import io.iohk.ethereum.blockchain.sync.SyncStateDownloaderActor._
import io.iohk.ethereum.blockchain.sync.SyncStateScheduler.SyncResponse
import io.iohk.ethereum.blockchain.sync.SyncStateSchedulerActor.{GetMissingNodes, MissingNodes}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, NodeData}
import io.iohk.ethereum.network.{Peer, PeerId}
import io.iohk.ethereum.utils.Config.SyncConfig

import scala.annotation.tailrec

/**
  * SyncStateDownloaderActor receives missing nodes from scheduler and makes sure that those nodes would be eventually retrieved.
  * It never ask ask two peers for the same nodes.
  * Another design choice would be to allow for duplicate node retrieval and ignore duplicates at scheduler level, but to do that
  * SyncStateDownloaderActor would need to keep track which peer was already asked for which node.
  */
class SyncStateDownloaderActor(
    val etcPeerManager: ActorRef,
    val peerEventBus: ActorRef,
    val syncConfig: SyncConfig,
    implicit val scheduler: Scheduler
) extends Actor
    with ActorLogging
    with BlacklistSupport
    with PeerListSupport
    with Timers {

  private def requestNodes(request: PeerRequest): ActorRef = {
    log.info(s"Requesting ${request.nodes.size} from peer ${request.peer}")
    val handler = context.actorOf(
      PeerRequestHandler.props[GetNodeData, NodeData](
        request.peer,
        syncConfig.peerResponseTimeout,
        etcPeerManager,
        peerEventBus,
        requestMsg = GetNodeData(request.nodes.toList),
        responseMsgCode = NodeData.code
      )
    )
    context.watchWith(handler, RequestTerminated(request.peer))
  }

  private def getFreePeers(state: DownloaderState) = {
    handshakedPeers.collect {
      case (peer, _) if !state.activeRequests.contains(peer.id) && !isBlacklisted(peer.id) => peer
    }
  }

  def checkPeerAvailabilityIfSomeDataQueued(state: DownloaderState): Unit = {
    if (state.nonDownloadedNodes.nonEmpty) {
      self ! GetMissingNodes(List())
    }
  }

  def handleRequestResults(scheduler: ActorRef, currentState: DownloaderState): Receive = {
    case ResponseReceived(peer, nodeData: NodeData, timeTaken) =>
      log.info("Received {} state nodes in {} ms", nodeData.values.size, timeTaken)
      context unwatch (sender())
      currentState.handleRequestSuccess(peer, nodeData) match {
        case (UnrequestedResponse, newState) =>
          log.debug("Received unexpected response from {}", peer.id)
          // just ignore unrequested stuff
          context.become(downloading(scheduler, newState))
        case (NoUsefulDataInResponse, newState) =>
          log.debug("Received no useful data from peer {}, blacklisting", peer)
          blacklist(peer.id, syncConfig.blacklistDuration, "Empty response")
          checkPeerAvailabilityIfSomeDataQueued(newState)
          context.become(downloading(scheduler, newState))
        case (UsefulData(responsesToProcess), newState) =>
          log.info("Received {} responses from peer {}", responsesToProcess.size, peer.id)
          val freePeersSize = getFreePeers(newState).size
          val numberOfNonActivelyDownloadedNodes = newState.nonDownloadedNodes.size
          log.debug(
            "Currently there are {} free peers, and {} nodes to download",
            freePeersSize,
            numberOfNonActivelyDownloadedNodes
          )
          val currentCapacity =
            ((freePeersSize * syncConfig.nodesPerRequest) - numberOfNonActivelyDownloadedNodes).max(0)
          scheduler ! MissingNodes(responsesToProcess, currentCapacity)
          // we got free peer lets re-schedule task assignment
          checkPeerAvailabilityIfSomeDataQueued(newState)
          context.become(downloading(scheduler, newState))
      }

    case PeerRequestHandler.RequestFailed(peer, reason) =>
      context unwatch (sender())
      log.debug(s"Request failed to peer {} due to {}", peer.id, reason)
      val newState = currentState.handleRequestFailure(peer)
      checkPeerAvailabilityIfSomeDataQueued(newState)
      if (handshakedPeers.contains(peer)) {
        blacklist(peer.id, syncConfig.blacklistDuration, reason)
      }
      context.become(downloading(scheduler, newState))

    case RequestTerminated(peer) =>
      log.debug(s"Request to {} terminated", peer.id)
      val newState = currentState.handleRequestFailure(peer)
      checkPeerAvailabilityIfSomeDataQueued(newState)
      context.become(downloading(scheduler, newState))
  }

  def idle: Receive = handleCommonMessages orElse { case RegisterScheduler =>
    log.debug("Scheduler registered, starting sync download")
    context.become(downloading(sender(), DownloaderState(Map.empty, Map.empty)))
  }

  def handleCommonMessages: Receive = handlePeerListMessages orElse handleBlacklistMessages

  def downloading(scheduler: ActorRef, currentState: DownloaderState): Receive =
    handleRequestResults(scheduler, currentState) orElse
      handleCommonMessages orElse {
        case GetMissingNodes(newNodesToGet) =>
          val freePeers = getFreePeers(currentState)
          if (freePeers.isEmpty) {
            log.info("No available peer, rescheduling request for retrieval")
            timers.startSingleTimer(CheckForPeersKey, GetMissingNodes(List.empty), syncConfig.syncRetryInterval)
            context.become(downloading(scheduler, currentState.scheduleNewNodesForRetrieval(newNodesToGet)))
          } else if (newNodesToGet.isEmpty && currentState.nodesToGet.isEmpty) {
            log.info("No available work, waiting for additional requests")
          } else {
            val nodesToGet = if (newNodesToGet.isEmpty) None else Some(newNodesToGet)
            val (newRequests, newState) =
              currentState.assignTasksToPeers(
                NonEmptyList.fromListUnsafe(freePeers.toList),
                nodesToGet,
                syncConfig.nodesPerRequest
              )
            log.info(
              "Creating {} new state node requests. Current request queue size is {}",
              newRequests.size,
              newState.nodesToGet.size
            )
            newRequests.foreach { request =>
              requestNodes(request)
            }
            context.become(downloading(scheduler, newState))
          }
        case CancelDownload =>
          context.become(idle)
      }

  override def receive: Receive = idle
}

object SyncStateDownloaderActor {
  def props(etcPeerManager: ActorRef, peerEventBus: ActorRef, syncConfig: SyncConfig, scheduler: Scheduler): Props = {
    Props(new SyncStateDownloaderActor(etcPeerManager, peerEventBus, syncConfig, scheduler))
  }
  final case object CancelDownload

  final case object CheckForPeersKey

  final case class RequestTerminated(to: Peer)

  final case class PeerRequest(peer: Peer, nodes: NonEmptyList[ByteString])

  final case object RegisterScheduler

  sealed trait ResponseProcessingResult

  case object UnrequestedResponse extends ResponseProcessingResult

  case object NoUsefulDataInResponse extends ResponseProcessingResult

  case class UsefulData(responses: List[SyncResponse]) extends ResponseProcessingResult

  final case class DownloaderState(
      activeRequests: Map[PeerId, NonEmptyList[ByteString]],
      nodesToGet: Map[ByteString, Option[PeerId]]
  ) {
    lazy val nonDownloadedNodes = nodesToGet.collect {
      case (hash, maybePeer) if maybePeer.isEmpty => hash
    }.toSeq

    def scheduleNewNodesForRetrieval(nodes: Seq[ByteString]): DownloaderState = {
      val newNodesToGet = nodes.foldLeft(nodesToGet) { case (map, node) =>
        if (map.contains(node)) {
          map
        } else {
          map + (node -> None)
        }
      }

      copy(nodesToGet = newNodesToGet)
    }

    private def addActiveRequest(peerRequest: PeerRequest): DownloaderState = {
      val newNodesToget = peerRequest.nodes.foldLeft(nodesToGet) { case (map, node) =>
        map + (node -> Some(peerRequest.peer.id))
      }

      copy(activeRequests = activeRequests + (peerRequest.peer.id -> peerRequest.nodes), nodesToGet = newNodesToget)
    }

    def handleRequestFailure(from: Peer): DownloaderState = {
      val requestedNodes = activeRequests(from.id)
      val newNodesToGet = requestedNodes.foldLeft(nodesToGet) { case (map, node) =>
        map + (node -> None)
      }

      copy(activeRequests = activeRequests - from.id, nodesToGet = newNodesToGet)
    }

    /**
      * Responses from peers should be delivered in order, but can contain gaps or can be not full, so we cannot fail
      * on first not matching response.
      * Matched responses are returned in correct order, the hashes to be rescheduled are returned in no particular order
      * as they will either way end up in map of hashes to be re-downloaded
      */
    def process(
        requested: NonEmptyList[ByteString],
        received: NonEmptyList[ByteString]
    ): (List[ByteString], List[SyncResponse]) = {
      @tailrec
      def go(
          remainingRequestedHashes: List[ByteString],
          nextResponse: SyncResponse,
          remainingResponses: List[ByteString],
          nonReceivedRequested: List[ByteString],
          processed: List[SyncResponse]
      ): (List[ByteString], List[SyncResponse]) = {
        if (remainingRequestedHashes.isEmpty) {
          (nonReceivedRequested, processed.reverse)
        } else {
          val nextRequestedHash = remainingRequestedHashes.head
          if (nextRequestedHash == nextResponse.hash) {
            if (remainingResponses.isEmpty) {
              val finalNonReceived = remainingRequestedHashes.tail ::: nonReceivedRequested
              val finalProcessed = nextResponse :: processed
              (finalNonReceived, finalProcessed.reverse)
            } else {
              val nexExpectedResponse = SyncResponse(kec256(remainingResponses.head), remainingResponses.head)
              go(
                remainingRequestedHashes.tail,
                nexExpectedResponse,
                remainingResponses.tail,
                nonReceivedRequested,
                nextResponse :: processed
              )
            }
          } else {
            go(
              remainingRequestedHashes.tail,
              nextResponse,
              remainingResponses,
              nextRequestedHash :: nonReceivedRequested,
              processed
            )
          }
        }
      }

      val firstReceivedResponse = SyncResponse(kec256(received.head), received.head)

      go(requested.toList, firstReceivedResponse, received.tail, List.empty, List.empty)
    }

    def handleRequestSuccess(from: Peer, receivedMessage: NodeData): (ResponseProcessingResult, DownloaderState) = {
      activeRequests
        .get(from.id)
        .map { requestedHashes =>
          if (receivedMessage.values.isEmpty) {
            val rescheduleRequestedHashes = requestedHashes.foldLeft(nodesToGet) { case (map, hash) =>
              map + (hash -> None)
            }
            (
              NoUsefulDataInResponse,
              copy(activeRequests = activeRequests - from.id, nodesToGet = rescheduleRequestedHashes)
            )
          } else {
            val (notReceived, received) =
              process(requestedHashes, NonEmptyList.fromListUnsafe(receivedMessage.values.toList))
            if (received.isEmpty) {
              val rescheduleRequestedHashes = notReceived.foldLeft(nodesToGet) { case (map, hash) =>
                map + (hash -> None)
              }
              (
                NoUsefulDataInResponse,
                copy(activeRequests = activeRequests - from.id, nodesToGet = rescheduleRequestedHashes)
              )
            } else {
              val afterNotReceive = notReceived.foldLeft(nodesToGet) { case (map, hash) => map + (hash -> None) }
              val afterReceived = received.foldLeft(afterNotReceive) { case (map, received) => map - received.hash }
              (UsefulData(received), copy(activeRequests = activeRequests - from.id, nodesToGet = afterReceived))
            }
          }
        }
        .getOrElse((UnrequestedResponse, this))
    }

    def assignTasksToPeers(
        peers: NonEmptyList[Peer],
        newNodes: Option[Seq[ByteString]],
        nodesPerPeerCapacity: Int
    ): (Seq[PeerRequest], DownloaderState) = {
      @tailrec
      def go(
          peersRemaining: List[Peer],
          nodesRemaining: Seq[ByteString],
          createdRequests: List[PeerRequest],
          currentState: DownloaderState
      ): (Seq[PeerRequest], DownloaderState) = {
        if (peersRemaining.isEmpty || nodesRemaining.isEmpty) {
          (createdRequests.reverse, currentState.scheduleNewNodesForRetrieval(nodesRemaining))
        } else {
          val nextPeer = peersRemaining.head
          val (nodes, nodesAfterAssignment) = nodesRemaining.splitAt(nodesPerPeerCapacity)
          val peerRequest = PeerRequest(nextPeer, NonEmptyList.fromListUnsafe(nodes.toList))
          go(
            peersRemaining.tail,
            nodesAfterAssignment,
            peerRequest :: createdRequests,
            currentState.addActiveRequest(peerRequest)
          )
        }
      }

      val currentNodesToDeliver = newNodes.map(nodes => nonDownloadedNodes ++ nodes).getOrElse(nonDownloadedNodes)
      if (currentNodesToDeliver.isEmpty) {
        (Seq(), this)
      } else {
        go(peers.toList, currentNodesToDeliver, List.empty, this)
      }
    }

  }

}
