package io.iohk.ethereum.blockchain.sync.fast

import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.blockchain.sync.fast.SyncStateScheduler.SyncResponse
import io.iohk.ethereum.blockchain.sync.fast.SyncStateSchedulerActor.{
  NoUsefulDataInResponse,
  PeerRequest,
  ResponseProcessingResult,
  UnrequestedResponse,
  UsefulData
}
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.network.p2p.messages.PV63.NodeData
import io.iohk.ethereum.network.{Peer, PeerId}

import scala.annotation.tailrec

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
    activeRequests
      .get(from.id)
      .map { requestedNodes =>
        val newNodesToGet = requestedNodes.foldLeft(nodesToGet) { case (map, node) =>
          map + (node -> None)
        }

        copy(activeRequests = activeRequests - from.id, nodesToGet = newNodesToGet)
      }
      .getOrElse(this)
  }

  /** Responses from peers should be delivered in order, but can contain gaps or can be not full, so we cannot fail
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

object DownloaderState {
  def apply(): DownloaderState = new DownloaderState(Map.empty, Map.empty)
}
