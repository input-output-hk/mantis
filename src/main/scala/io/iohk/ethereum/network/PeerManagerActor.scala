package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import org.spongycastle.util.encoders.Hex

class PeerManagerActor(nodeInfo: NodeInfo) extends Actor with ActorLogging {

  import PeerManagerActor._

  var peers: Map[String, Peer] = Map.empty

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  override def receive: Receive = {
    case HandlePeerConnection(connection, remoteAddress) =>
      val peer = createPeer(remoteAddress)
      log.info("Peer {} handling incoming peer connection from {}", peer.id, remoteAddress)
      peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
      sender() ! PeerCreated(peer)

    case ConnectToPeer(uri) =>
      val peer = createPeer(new InetSocketAddress(uri.getHost, uri.getPort))
      peer.ref ! PeerActor.ConnectTo(uri)
      sender() ! PeerCreated(peer)

    case GetPeers =>
      sender() ! peers

    case Terminated(ref) =>
      peers -= ref.path.name

    case StartFastDownload(uri) =>
      peers.values.headOption.foreach { p =>
        p.ref ! PeerActor.StartFastSync(
          startBlockHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3")), //genesis block
          targetBlockHash = ByteString(Hex.decode("ca2b65cf841b7acc2548977ad69a3e118940d0934cdbf2d3645c44bdf5023465"))
        )
      }
  }

  def createPeer(addr: InetSocketAddress): Peer = {
    val id = addr.toString.filterNot(_ == '/')
    val ref = context.actorOf(PeerActor.props(nodeInfo, _.actorOf(RLPxConnectionHandler.props(nodeInfo), "rlpx-connection")), id)
    context watch ref
    val peer = Peer(id, addr, ref)
    peers += id -> peer
    peer
  }

}

object PeerManagerActor {
  def props(nodeInfo: NodeInfo): Props =
    Props(new PeerManagerActor(nodeInfo))

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case class StartFastDownload(uri: URI)


  case class Peer(id: String, remoteAddress: InetSocketAddress, ref: ActorRef)
  case class PeerCreated(peer: Peer)

  case object GetPeers
}
