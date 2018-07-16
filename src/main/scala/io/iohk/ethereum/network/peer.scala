package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import io.iohk.ethereum.utils.ToRiemann
import io.iohk.ethereum.utils.Riemann
import io.riemann.riemann.client.EventDSL

case class PeerId(value: String) extends AnyVal

case class Peer(remoteAddress: InetSocketAddress, ref: ActorRef, incomingConnection: Boolean) extends ToRiemann {
  def id: PeerId = PeerId(ref.path.name)
  override def toRiemann: EventDSL =
    Riemann.ok("peer")
      .attribute("remote-host", remoteAddress.getAddress.getHostAddress)
      .attribute("remote-port", remoteAddress.getPort.toString)
      .attribute("incoming-connection", incomingConnection.toString)
}
