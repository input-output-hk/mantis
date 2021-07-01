package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.util.Timeout

import monix.eval.Task

import scala.concurrent.duration._

import io.iohk.ethereum.jsonrpc.AkkaTaskOps._
import io.iohk.ethereum.jsonrpc.DebugService.ListPeersInfoRequest
import io.iohk.ethereum.jsonrpc.DebugService.ListPeersInfoResponse
import io.iohk.ethereum.network.EtcPeerManagerActor
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfoResponse
import io.iohk.ethereum.network.Peer
import io.iohk.ethereum.network.PeerActor
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.network.PeerManagerActor.Peers

object DebugService {
  case class ListPeersInfoRequest()
  case class ListPeersInfoResponse(peers: List[PeerInfo])
}

class DebugService(peerManager: ActorRef, etcPeerManager: ActorRef) {

  def listPeersInfo(getPeersInfoRequest: ListPeersInfoRequest): ServiceResponse[ListPeersInfoResponse] =
    for {
      ids <- getPeerIds
      peers <- Task.traverse(ids)(getPeerInfo)
    } yield Right(ListPeersInfoResponse(peers.flatten))

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
}
