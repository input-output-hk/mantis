package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import io.iohk.ethereum.utils.ToRiemann
import io.iohk.ethereum.utils.Riemann

case class PeerId(value: String) extends AnyVal

object PeerId {
  implicit val peerIdToRiemann: ToRiemann[PeerId] =
    peerId => Riemann.ok("peer-id").attribute("id", peerId.value)
}

case class Peer(remoteAddress: InetSocketAddress, ref: ActorRef, incomingConnection: Boolean) {
  def id: PeerId = PeerId(ref.path.name)
}

object Peer {

  implicit val peerToRiemann: ToRiemann[Peer] =
    peer => Riemann.ok("peer")
      .attribute("remote-host", peer.remoteAddress.getAddress.getHostAddress)
      .attribute("remote-port", peer.remoteAddress.getPort.toString)
      .attribute("incoming-connection", peer.incomingConnection.toString)

}
