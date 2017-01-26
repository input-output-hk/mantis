package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.util.ByteString
import io.iohk.ethereum.network.FastSyncActor.StartSync
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders

class FastSyncActor(peerActor: ActorRef) extends Actor with ActorLogging {

  val blocksPerMessage = 10

  def handleTerminated: Receive = {
    case _: Terminated =>
      log.info("FastSync actor terminated")
      context stop self
  }

  override def receive: Receive = handleTerminated orElse {
    case StartSync(hash) =>
      peerActor ! PeerActor.SendMessage(GetBlockHeaders(Right(hash), blocksPerMessage, 0, reverse = false))
  }
}

object FastSyncActor {
  def props(peerActor: ActorRef): Props = Props(new FastSyncActor(peerActor))

  case class StartSync(blockHash: ByteString)

}
