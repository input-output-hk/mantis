package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.NotUsed
import akka.actor.ActorRef
import akka.stream.scaladsl.Source
import akka.util.ByteString

import io.iohk.ethereum.blockchain.sync.Blacklist.BlacklistId
import io.iohk.ethereum.network.p2p.Message

final case class PeerId(value: String) extends BlacklistId

object PeerId {
  def fromRef(ref: ActorRef): PeerId = PeerId(ref.path.name)
}

final case class Peer(
    id: PeerId,
    remoteAddress: InetSocketAddress,
    ref: ActorRef,
    incomingConnection: Boolean,
    source: Source[Message, NotUsed] = Source.empty,
    nodeId: Option[ByteString] = None,
    createTimeMillis: Long = System.currentTimeMillis
)
