package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.blockchain.Blockchain
import io.iohk.ethereum.db.storage.MptNodeStorage
import io.iohk.ethereum.utils.{Config, NodeStatus}

class PeerManagerActor(
    nodeStatusHolder: Agent[NodeStatus],
    peerFactory: (ActorContext, InetSocketAddress) => ActorRef)
  extends Actor with ActorLogging {

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
      val peerAddresses = peers.values.map(_.remoteAddress).toSet
      val nodesToConnect = bootstrapNodes
        .map(new URI(_))
        .filterNot(uri => peerAddresses.contains(new InetSocketAddress(uri.getHost, uri.getPort)))

      if (nodesToConnect.nonEmpty) {
        log.info("Trying to connect to {} bootstrap nodes", nodesToConnect.size)
        nodesToConnect.foreach(self ! ConnectToPeer(_))
      }
    case StartFastDownload(uri, targetHash, blockchain, mptNodeStorage) =>
      peers.find { case (_, Peer(remoteAddress, _)) => remoteAddress.getHostString == uri.getHost && remoteAddress.getPort == uri.getPort }
        .map(_._2)
        .foreach { p => p.ref ! PeerActor.StartFastSync(targetHash, blockchain, mptNodeStorage) }
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
  def props(nodeStatusHolder: Agent[NodeStatus]): Props =
    Props(new PeerManagerActor(nodeStatusHolder, peerFactory(nodeStatusHolder)))

  def peerFactory(nodeStatusHolder: Agent[NodeStatus]): (ActorContext, InetSocketAddress) => ActorRef = { (ctx, addr) =>
    val id = addr.toString.filterNot(_ == '/')
    ctx.actorOf(PeerActor.props(nodeStatusHolder), id)
  }

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case class StartFastDownload(uri: URI, targetBlockHash: ByteString, blockchain: Blockchain, mptNodeStorage: MptNodeStorage)

  case class Peer(remoteAddress: InetSocketAddress, ref: ActorRef) {
    def id: String = ref.path.name
  }

  case class PeerCreated(peer: Peer)

  case object GetPeers

  private case object ScanBootstrapNodes
}
