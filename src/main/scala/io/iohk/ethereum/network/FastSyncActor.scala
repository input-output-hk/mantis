package io.iohk.ethereum.network

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString

class FastSyncActor(peerActor: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = ???
}

object FastSyncActor {
  def props(peerActor: ActorRef): Props = Props(new FastSyncActor(peerActor))

  case class StartSync(blockHash: ByteString)
}
