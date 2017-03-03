package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock

class BlockBroadcastMaxBlockRequestHandler(peer: ActorRef, newBlocks: Seq[NewBlock]) extends Actor with ActorLogging  {

  override def preStart(): Unit = {
    context watch peer
    peer ! PeerActor.GetMaxBlockNumber(self)
  }

  override def receive: Receive = {
    case PeerActor.MaxBlockNumber(maxBlockNumber) =>
      log.info("Sending NewBlockMessage {} to {}", newBlocks, peer.path.name)
      newBlocks.foreach{ newBlockMsg =>
        if (maxBlockNumber < newBlockMsg.block.header.number) peer ! PeerActor.SendMessage(newBlockMsg)
      }
      context unwatch context.parent
      context stop self

    case Terminated(_) => context stop self
  }

}

object BlockBroadcastMaxBlockRequestHandler {
  def props(peer: ActorRef,
            newBlocks: Seq[NewBlock]): Props = {
    Props(new BlockBroadcastMaxBlockRequestHandler(peer, newBlocks))
  }
}
