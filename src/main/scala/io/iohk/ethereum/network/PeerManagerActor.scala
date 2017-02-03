package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}
import java.util.UUID

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.utils.NodeStatus

class PeerManagerActor(nodeStatusHolder: Agent[NodeStatus]) extends Actor with ActorLogging {

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

    case StartFastDownload(uri,targetHash) =>
      peers.get(keyForURI(uri)).foreach { p => p.ref ! PeerActor.StartFastSync(targetHash)}
  }

  def createPeer(addr: InetSocketAddress): Peer = {
    val id = addr.toString.filterNot(_ == '/')
    //todo temporal fix, fix this in a way that makes sense
    val ref = context.actorOf(PeerActor.props(nodeStatusHolder, _.actorOf(RLPxConnectionHandler.props(nodeStatusHolder().key),
      s"rlpx-connection-${UUID.randomUUID().toString}")), id)
    context watch ref
    val peer = Peer(id, addr, ref)
    peers += id -> peer
    peer
  }

  private def keyForURI(uri: URI): String = {
    new InetSocketAddress(uri.getHost, uri.getPort).toString.filterNot(_ == '/')
  }
}

object PeerManagerActor {
  def props(nodeStatusHolder: Agent[NodeStatus]): Props =
    Props(new PeerManagerActor(nodeStatusHolder))

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case class StartFastDownload(uri: URI, targetBlockHash: ByteString)

  case class Peer(id: String, remoteAddress: InetSocketAddress, ref: ActorRef)
  case class PeerCreated(peer: Peer)

  case object GetPeers
}
