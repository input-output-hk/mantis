package io.iohk.ethereum.network

import akka.actor.ActorRef
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistId

import java.net.InetSocketAddress

final case class PeerId(value: String) extends BlacklistId

object PeerId {
  def fromRef(ref: ActorRef): PeerId = PeerId(ref.path.name)
}

final case class Peer(
    id: PeerId,
    remoteAddress: InetSocketAddress,
    ref: ActorRef,
    incomingConnection: Boolean,
    nodeId: Option[ByteString] = None,
    createTimeMillis: Long = System.currentTimeMillis
)
