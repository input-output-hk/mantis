package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.agent.Agent
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.utils.{Config, NodeStatus}

class PeerManagerActor(nodeStatusHolder: Agent[NodeStatus]) extends Actor with ActorLogging {

  import PeerManagerActor._
  import Config.Network.Discovery._

  var peers: Map[String, Peer] = Map.empty

  context.system.scheduler.schedule(0.seconds, bootstrapNodesScanInterval, self, ScanBootstrapNodes)

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

    case ScanBootstrapNodes =>
      val peerAddresses = peers.values.map(_.remoteAddress).toSeq
      val nodesToConnect = bootstrapNodes
        .map(new URI(_))
        .filterNot(uri => peerAddresses.contains(new InetSocketAddress(uri.getHost, uri.getPort)))

      log.info("Trying to connect to {} bootstrap nodes", nodesToConnect.size)
      nodesToConnect.foreach(self ! ConnectToPeer(_))
  }

  def createPeer(addr: InetSocketAddress): Peer = {
    val id = addr.toString.filterNot(_ == '/')
    val ref = context.actorOf(PeerActor.props(nodeStatusHolder, _.actorOf(RLPxConnectionHandler.props(nodeStatusHolder().key), "rlpx-connection")), id)
    context watch ref
    val peer = Peer(id, addr, ref)
    peers += id -> peer
    peer
  }

}

object PeerManagerActor {
  def props(nodeStatusHolder: Agent[NodeStatus]): Props =
    Props(new PeerManagerActor(nodeStatusHolder))

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)
  case class ConnectToPeer(uri: URI)

  case class Peer(id: String, remoteAddress: InetSocketAddress, ref: ActorRef)
  case class PeerCreated(peer: Peer)

  case object GetPeers

  private case object ScanBootstrapNodes
}
