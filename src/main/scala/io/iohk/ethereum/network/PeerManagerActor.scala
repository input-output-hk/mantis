package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}
import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPEncoder
import org.spongycastle.util.encoders.Hex

import scala.collection.immutable.HashMap

class PeerManagerActor(nodeInfo: NodeInfo) extends Actor with ActorLogging {

  import PeerManagerActor._

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  var actor: ActorRef = _

  override def receive: Receive = {
    case HandlePeerConnection(connection, remoteAddress) =>
      val peer = createPeer()
      log.info("Peer {} handling incoming peer connection from {}", peer.path.name, remoteAddress)
      peer ! PeerActor.HandleConnection(connection)

    case ConnectToPeer(uri) =>
      val actor = createPeer()
      actor ! PeerActor.ConnectTo(uri)
      this.actor = actor

    case StartFastDownload(uri) =>
      actor ! PeerActor.StartFastSync(ByteString(Hex.decode("a2ceafb458490d595d8418fa88a3229b7db08100cb84ee6685c1c1227435dd8b")))
  }

  def createPeer(): ActorRef = {
    val id = UUID.randomUUID.toString
    context.actorOf(PeerActor.props(nodeInfo), id)
  }
}

object PeerManagerActor {
  def props(nodeInfo: NodeInfo): Props =
    Props(new PeerManagerActor(nodeInfo))

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)
  case class ConnectToPeer(uri: URI)

  case class StartFastDownload(uri: URI)
}
