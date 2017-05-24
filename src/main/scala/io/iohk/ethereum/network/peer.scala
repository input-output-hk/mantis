package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorRef

case class PeerId(value: String) extends AnyVal

case class Peer(remoteAddress: InetSocketAddress, ref: ActorRef) {
  def id: PeerId = PeerId(ref.path.name)
}
