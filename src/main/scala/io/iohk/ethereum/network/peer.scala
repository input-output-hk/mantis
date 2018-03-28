package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId

case class PeerId(value: String) extends BlackListId

case class Peer(remoteAddress: InetSocketAddress, ref: ActorRef, incomingConnection: Boolean) {
  def id: PeerId = PeerId(ref.path.name)
}
