package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.jsonrpc.DebugService.{
  GetSpecificBlockRequest,
  GetSpecificBlockResponse,
  ListPeersInfoRequest,
  ListPeersInfoResponse
}
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, PeerInfoResponse}
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerActor, PeerId, PeerManagerActor}
import io.iohk.ethereum.utils.Hex
import monix.eval.Task

import scala.concurrent.duration._

object DebugService {
  case class ListPeersInfoRequest()
  case class ListPeersInfoResponse(peers: List[PeerInfo])

  case class GetSpecificBlockRequest(hash: ByteString, peerId: PeerId)
  case class GetSpecificBlockResponse()
}

class DebugService(peerManager: ActorRef, etcPeerManager: ActorRef) {

  def listPeersInfo(getPeersInfoRequest: ListPeersInfoRequest): ServiceResponse[ListPeersInfoResponse] = {
    for {
      ids <- getPeerIds
      peers <- Task.traverse(ids)(getPeerInfo)
    } yield Right(ListPeersInfoResponse(peers.flatten))
  }

  private def getPeerIds: Task[List[PeerId]] = {
    implicit val timeout: Timeout = Timeout(5.seconds)

    peerManager
      .askFor[Peers](PeerManagerActor.GetPeers)
      .onErrorRecover { case _ => Peers(Map.empty[Peer, PeerActor.Status]) }
      .map(_.peers.keySet.map(_.id).toList)
  }

  private def getPeerInfo(peer: PeerId): Task[Option[PeerInfo]] = {
    implicit val timeout: Timeout = Timeout(5.seconds)

    etcPeerManager
      .askFor[PeerInfoResponse](EtcPeerManagerActor.PeerInfoRequest(peer))
      .map(resp => resp.peerInfo)
  }

  def getSpecificBlock(getSpecificBlockRequest: GetSpecificBlockRequest): ServiceResponse[GetSpecificBlockResponse] = {
    peerManager.tell(
      PeerManagerActor.SendMessage(
        GetBlockHeaders(
          Right(getSpecificBlockRequest.hash),
          1,
          0,
          false
        ),
        getSpecificBlockRequest.peerId
      ),
      peerManager
    )
    Task(Right(GetSpecificBlockResponse()))
  }
}
