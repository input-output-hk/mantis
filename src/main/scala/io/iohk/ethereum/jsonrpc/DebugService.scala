package io.iohk.ethereum.jsonrpc

import akka.actor.ActorRef
import akka.pattern._
import akka.util.Timeout
import io.iohk.ethereum.jsonrpc.DebugService.{ListPeersInfoRequest, ListPeersInfoResponse}
import io.iohk.ethereum.network.EtcPeerManagerActor.{PeerInfo, PeerInfoResponse}
import io.iohk.ethereum.network.PeerManagerActor.Peers
import io.iohk.ethereum.network.{EtcPeerManagerActor, Peer, PeerActor, PeerId, PeerManagerActor}
import monix.eval.Task

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object DebugService {

  case class ListPeersInfoRequest()
  case class ListPeersInfoResponse(peers: List[PeerInfo])

}

class DebugService(peerManager: ActorRef, etcPeerManager: ActorRef) {

  def listPeersInfo(getPeersInfoRequest: ListPeersInfoRequest): ServiceResponse[ListPeersInfoResponse] = {
    val result = for {
      ids <- getPeerIds
      peers <- Task.traverse(ids)(getPeerInfo)
    } yield ListPeersInfoResponse(peers.flatten)

    Task.from(result.map(Right(_)))
  }

  private def getPeerIds: Task[List[PeerId]] = {
    implicit val timeout: Timeout = Timeout(5.seconds)

    Task.fromFuture(
      (peerManager ? PeerManagerActor.GetPeers)
        .mapTo[Peers]
        .recover { case _ => Peers(Map.empty[Peer, PeerActor.Status]) }
        .map(_.peers.keySet.map(_.id).toList)
    )
  }

  private def getPeerInfo(peer: PeerId): Task[Option[PeerInfo]] = {
    implicit val timeout: Timeout = Timeout(5.seconds)

    Task.fromFuture {
      (etcPeerManager ? EtcPeerManagerActor.PeerInfoRequest(peer))
        .mapTo[PeerInfoResponse]
        .collect { case PeerInfoResponse(info) => info }
    }
  }
}
