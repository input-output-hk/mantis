package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.agent.Agent
import io.iohk.ethereum.network.PeerActor.{FastSyncHostConfiguration, PeerConfiguration}
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.utils.{Config, NodeStatus}

class PeerManagerActor(
    nodeStatusHolder: Agent[NodeStatus],
    peerFactory: (ActorContext, InetSocketAddress) => ActorRef,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor with ActorLogging {

  import PeerManagerActor._
  import Config.Network.Discovery._

  var peers: Map[String, Peer] = Map.empty

  private def scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  scheduler.schedule(0.seconds, bootstrapNodesScanInterval, self, ScanBootstrapNodes)

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  override def receive: Receive = {
    case HandlePeerConnection(connection, remoteAddress) =>
      val peer = createPeer(remoteAddress)
      log.debug("Peer {} handling incoming peer connection from {}", peer.id, remoteAddress)
      peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
      sender() ! PeerCreated(peer)

    case ConnectToPeer(uri) =>
      val peer = createPeer(new InetSocketAddress(uri.getHost, uri.getPort))
      peer.ref ! PeerActor.ConnectTo(uri)
      sender() ! PeerCreated(peer)

    case GetPeers =>
      sender() ! PeersResponse(peers.values.toSeq)

    case Terminated(ref) =>
      peers -= ref.path.name

    case ScanBootstrapNodes =>
      val peerAddresses = peers.values.map(_.remoteAddress).toSet
      val nodesToConnect = bootstrapNodes
        .map(new URI(_))
        .filterNot(uri => peerAddresses.contains(new InetSocketAddress(uri.getHost, uri.getPort)))

      if (nodesToConnect.nonEmpty) {
        log.info("Trying to connect to {} bootstrap nodes", nodesToConnect.size)
        nodesToConnect.foreach(self ! ConnectToPeer(_))
      }
  }

  def createPeer(addr: InetSocketAddress): Peer = {
    val ref = peerFactory(context, addr)
    context watch ref
    val peer = Peer(addr, ref)
    peers += peer.id -> peer
    peer
  }

}

object PeerManagerActor {
  def props(nodeStatusHolder: Agent[NodeStatus], peerConfiguration: PeerConfiguration, storage: Blockchain): Props =
    Props(new PeerManagerActor(nodeStatusHolder, peerFactory(nodeStatusHolder, peerConfiguration, storage)))

  def peerFactory(nodeStatusHolder: Agent[NodeStatus], peerConfiguration: PeerConfiguration,
    storage: Blockchain): (ActorContext, InetSocketAddress) => ActorRef = {
    (ctx, addr) =>
      val id = addr.toString.filterNot(_ == '/')
      ctx.actorOf(PeerActor.props(nodeStatusHolder, peerConfiguration, storage), id)
  }

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case class Peer(remoteAddress: InetSocketAddress, ref: ActorRef) {
    def id: String = ref.path.name
  }

  case class PeerCreated(peer: Peer)

  case object GetPeers
  case class PeersResponse(peers: Seq[Peer])

  private case object ScanBootstrapNodes
}
