package io.iohk.ethereum.network

import java.net.URI
import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorRef, OneForOneStrategy, Props}
import org.spongycastle.crypto.AsymmetricCipherKeyPair

class PeerManagerActor(nodeKey: AsymmetricCipherKeyPair) extends Actor {

  import PeerManagerActor._

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  override def receive = {
    case HandlePeerConnection(connection) =>
      val peer = context.actorOf(PeerActor.props(UUID.randomUUID.toString, nodeKey))
      peer ! PeerActor.HandleConnection(connection)

    case ConnectToPeer(uri) =>
      val peer = context.actorOf(PeerActor.props(UUID.randomUUID.toString, nodeKey))
      peer ! PeerActor.ConnectTo(uri)
  }

}

object PeerManagerActor {
  def props(nodeKey: AsymmetricCipherKeyPair) = Props(new PeerManagerActor(nodeKey))

  case class HandlePeerConnection(connection: ActorRef)
  case class ConnectToPeer(uri: URI)
}
