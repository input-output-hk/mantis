package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}
import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import org.spongycastle.crypto.AsymmetricCipherKeyPair

class PeerManagerActor(nodeKey: AsymmetricCipherKeyPair) extends Actor with ActorLogging {

  import PeerManagerActor._

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  override def receive: Receive = {
    case HandlePeerConnection(connection, remoteAddress) =>
      val peer = createPeer()
      log.info("Peer {} handling incoming peer connection from {}", peer.path.name, remoteAddress)
      peer ! PeerActor.HandleConnection(connection)

    case ConnectToPeer(uri) =>
      createPeer() ! PeerActor.ConnectTo(uri)
  }

  def createPeer(): ActorRef = {
    val id = UUID.randomUUID.toString
    context.actorOf(PeerActor.props(nodeKey), id)
  }
}

object PeerManagerActor {
  def props(nodeKey: AsymmetricCipherKeyPair): Props =
    Props(new PeerManagerActor(nodeKey))

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)
  case class ConnectToPeer(uri: URI)

  case object GetPeers
  case class Peer(id: String, ref: ActorRef, address: InetSocketAddress)
}
