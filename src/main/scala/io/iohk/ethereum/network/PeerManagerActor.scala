package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.util.Timeout
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.agent.Agent
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.{MessageDecoder, MessageSerializable}
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.utils.NodeStatus

class PeerManagerActor(
    peerEventBus: ActorRef,
    peerDiscoveryManager: ActorRef,
    peerConfiguration: PeerConfiguration,
    knownNodesManager: ActorRef,
    peerFactory: (ActorContext, InetSocketAddress, Boolean) => ActorRef,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor with ActorLogging with Stash {

  import akka.pattern.{ask, pipe}
  import PeerManagerActor._

  var peers: Map[PeerId, Peer] = Map.empty
  private def incomingPeers = peers.filter { case (_, p) => p.incomingConnection }
  private def outgoingPeers = peers.filter { case (_, p) => !p.incomingConnection }

  private def scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  override def receive: Receive = {

    case StartConnecting =>
      scheduleNodesUpdate()
      knownNodesManager ! KnownNodesManager.GetKnownNodes
      context become listening
      unstashAll()

    case _ =>
      stash()

  }

  private def scheduleNodesUpdate(): Unit = {
    scheduler.schedule(peerConfiguration.updateNodesInitialDelay, peerConfiguration.updateNodesInterval) {
      peerDiscoveryManager ! PeerDiscoveryManager.GetDiscoveredNodes
    }
  }

  def listening: Receive = handleCommonMessages orElse {
    case KnownNodesManager.KnownNodes(nodes) =>
      val nodesToConnect = nodes.take(peerConfiguration.maxPeers)

      if (nodesToConnect.nonEmpty) {
        log.info("Trying to connect to {} known nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n))
      }

    case HandlePeerConnection(connection, remoteAddress) =>
      handleConnection(connection, remoteAddress)

    case msg: ConnectToPeer =>
      connect(msg.uri)

    case PeerDiscoveryManager.DiscoveredNodes(nodes) =>
      val peerAddresses = outgoingPeers.values.map(_.remoteAddress).toSet

      val nodesToConnect = nodes
        .filterNot(n => peerAddresses.contains(n.addr)) // not already connected to
        .toSeq
        .sortBy(-_.addTimestamp)
        .take(peerConfiguration.maxPeers - peerAddresses.size)

      log.debug(s"Discovered ${nodes.size} nodes, connected to ${peers.size}/${peerConfiguration.maxPeers + peerConfiguration.maxIncomingPeers}. " +
        s"Trying to connect to ${nodesToConnect.size} more nodes.")

      if (nodesToConnect.nonEmpty) {
        log.info("Trying to connect to {} nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n.toUri))
      }
  }

  def handleConnection(connection: ActorRef, remoteAddress: InetSocketAddress): Unit = {
    if (!isConnectionHandled(remoteAddress)) {
      val peer = createPeer(remoteAddress, incomingConnection = true)
      peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)

      if (incomingPeers.size >= peerConfiguration.maxPeers) {
        peer.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)
        log.info("Maximum number of incoming peer connections reached.")
      }
    } else {
      log.info("Another connection with {} is already opened. Disconnecting.", remoteAddress)
      connection ! PoisonPill
    }
  }

  def connect(uri: URI): Unit = {
    if (!isConnectionHandled(new InetSocketAddress(uri.getHost, uri.getPort))) {
      if (outgoingPeers.size < peerConfiguration.maxPeers) {
        val addr = new InetSocketAddress(uri.getHost, uri.getPort)
        val peer = createPeer(addr, incomingConnection = false)
        peer.ref ! PeerActor.ConnectTo(uri)
      } else {
        log.info("Maximum number of connected peers reached.")
      }
    } else {
      log.info("Maximum number of connected peers reached. Not connecting to {}", uri)
    }
  }

  def isConnectionHandled(addr: InetSocketAddress): Boolean = {
    peers.values.map(_.remoteAddress).toSet.contains(addr)
  }

  def handleCommonMessages: Receive = {
    case GetPeers =>
      getPeers().pipeTo(sender())

    case SendMessage(message, peerId) if peers.contains(peerId) =>
      val peer = peers(peerId)
      peer.ref ! PeerActor.SendMessage(message)

    case Terminated(ref) =>
      val maybePeerId = peers.collect{ case (id, Peer(_, peerRef, _)) if peerRef == ref => id }
      maybePeerId.foreach{ peerId =>
        peerEventBus ! Publish(PeerDisconnected(peerId))
        peers -= peerId
      }
  }

  def createPeer(addr: InetSocketAddress, incomingConnection: Boolean): Peer = {
    val ref = peerFactory(context, addr, incomingConnection)
    context watch ref
    val peer = Peer(addr, ref, incomingConnection)
    peers += peer.id -> peer
    peer
  }

  def getPeers(): Future[Peers] = {
    implicit val timeout = Timeout(2.seconds)

    Future.traverse(peers.values) { peer =>
      (peer.ref ? PeerActor.GetStatus)
        .mapTo[PeerActor.StatusResponse]
        .map(sr => (peer, sr.status))
    }.map(r => Peers.apply(r.toMap))
  }

}

object PeerManagerActor {
  def props[R <: HandshakeResult](nodeStatusHolder: Agent[NodeStatus],
                                  peerDiscoveryManager: ActorRef,
                                  peerConfiguration: PeerConfiguration,
                                  peerMessageBus: ActorRef,
                                  knownNodesManager: ActorRef,
                                  handshaker: Handshaker[R],
                                  authHandshaker: AuthHandshaker,
                                  messageDecoder: MessageDecoder): Props =
    Props(new PeerManagerActor(peerMessageBus, peerDiscoveryManager, peerConfiguration,
      knownNodesManager = knownNodesManager,
      peerFactory = peerFactory(nodeStatusHolder, peerConfiguration, peerMessageBus, knownNodesManager, handshaker, authHandshaker, messageDecoder)))

  def peerFactory[R <: HandshakeResult](nodeStatusHolder: Agent[NodeStatus],
                                        peerConfiguration: PeerConfiguration,
                                        peerEventBus: ActorRef,
                                        knownNodesManager: ActorRef,
                                        handshaker: Handshaker[R],
                                        authHandshaker: AuthHandshaker,
                                        messageDecoder: MessageDecoder): (ActorContext, InetSocketAddress, Boolean) => ActorRef = {
    (ctx, addr, incomingConnection) =>
      val id = addr.toString.filterNot(_ == '/')
      ctx.actorOf(PeerActor.props(addr, nodeStatusHolder, peerConfiguration, peerEventBus,
        knownNodesManager, incomingConnection, handshaker, authHandshaker, messageDecoder), id)
  }

  trait PeerConfiguration {
    val connectRetryDelay: FiniteDuration
    val connectMaxRetries: Int
    val disconnectPoisonPillTimeout: FiniteDuration
    val waitForHelloTimeout: FiniteDuration
    val waitForStatusTimeout: FiniteDuration
    val waitForChainCheckTimeout: FiniteDuration
    val fastSyncHostConfiguration: FastSyncHostConfiguration
    val rlpxConfiguration: RLPxConfiguration
    val maxPeers: Int
    val maxIncomingPeers: Int
    val networkId: Int
    val updateNodesInitialDelay: FiniteDuration
    val updateNodesInterval: FiniteDuration
  }

  trait FastSyncHostConfiguration {
    val maxBlocksHeadersPerMessage: Int
    val maxBlocksBodiesPerMessage: Int
    val maxReceiptsPerMessage: Int
    val maxMptComponentsPerMessage: Int
  }

  case object StartConnecting

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case object GetPeers
  case class Peers(peers: Map[Peer, PeerActor.Status]) {
    def handshaked: Seq[Peer] = peers.collect{ case (peer, Handshaked) => peer }.toSeq
  }

  case class SendMessage(message: MessageSerializable, peerId: PeerId)
}
