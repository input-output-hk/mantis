package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId

case class PeerId(value: String) extends BlackListId

object PeerId {
  def fromRef(ref: ActorRef): PeerId = PeerId(ref.path.name)
}

case class Peer(
    remoteAddress: InetSocketAddress,
    ref: ActorRef,
    incomingConnection: Boolean,
    nodeId: Option[ByteString] = None
) {
  // FIXME PeerId should be actual peerId i.e id derived form node public key
  def id: PeerId = PeerId.fromRef(ref)
}
