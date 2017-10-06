package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.Timeout
import io.iohk.ethereum.network.PeerActor.IncomingConnectionHandshakeSuccess
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.PeerDisconnected
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.discovery.PeerDiscoveryManager
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.{MessageDecoder, MessageSerializable}
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class PeerManagerActor(
    peerEventBus: ActorRef,
    peerDiscoveryManager: ActorRef,
    peerConfiguration: PeerConfiguration,
    knownNodesManager: ActorRef,
    peerFactory: (ActorContext, InetSocketAddress, Boolean) => ActorRef,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor with ActorLogging with Stash {

  import PeerManagerActor._
  import akka.pattern.{ask, pipe}

  val managerState = new PeerManagerState(Map.empty, Map.empty)

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
      peerDiscoveryManager ! PeerDiscoveryManager.GetDiscoveredNodesInfo
    }
  }

  def listening: Receive = handleCommonMessages orElse {
    case KnownNodesManager.KnownNodes(nodes) =>
      val nodesToConnect = nodes.take(peerConfiguration.maxPeers)

      if (nodesToConnect.nonEmpty) {
        log.debug("Trying to connect to {} known nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n))
      }

    case HandlePeerConnection(connection, remoteAddress) =>
      handleConnection(connection, remoteAddress)

    case msg: ConnectToPeer =>
      connect(msg.uri)

    case PeerDiscoveryManager.DiscoveredNodesInfo(nodesInfo) =>
      val peerAddresses = managerState.outgoingPeers.values.map(_.remoteAddress).toSet

      val nodesToConnect = nodesInfo
        .filterNot(n => peerAddresses.contains(n.node.addr)) // not already connected to
        .toSeq
        .sortBy(-_.addTimestamp)
        .take(peerConfiguration.maxPeers - peerAddresses.size)

      log.debug(
        s"Discovered ${nodesInfo.size} nodes, connected to ${managerState.peers.size}/${peerConfiguration.maxPeers + peerConfiguration.maxIncomingPeers}. " +
        s"Trying to connect to ${nodesToConnect.size} more nodes."
      )

      if (nodesToConnect.nonEmpty) {
        log.debug("Trying to connect to {} nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n.node.toUri))
      }
  }

  def handleConnection(connection: ActorRef, remoteAddress: InetSocketAddress): Unit = {
    if (!managerState.isConnectionHandled(remoteAddress)) {
      if (managerState.pendingIncomingPeers.size < peerConfiguration.maxPendingPeers) {
        val peer = createPeer(remoteAddress, incomingConnection = true)
        peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
      } else {
        log.debug("Maximum number of pending incoming peers reached.")
        connection ! PoisonPill
      }
    } else {
      log.debug("Another connection with {} is already opened. Disconnecting.", remoteAddress)
      connection ! PoisonPill
    }
  }

  def connect(uri: URI): Unit = {
    val addr = new InetSocketAddress(uri.getHost, uri.getPort)
    if (!managerState.isConnectionHandled(addr)) {
      if (managerState.outgoingPeers.size < peerConfiguration.maxPeers) {
        val peer = createPeer(addr, incomingConnection = false)
        peer.ref ! PeerActor.ConnectTo(uri)
      } else {
        log.debug("Maximum number of connected peers reached.")
      }
    } else {
      log.debug("Another connection with {} is already opened", uri)
    }
  }

  def handleCommonMessages: Receive = {
    case GetPeers =>
      getPeers().pipeTo(sender())

    case SendMessage(message, peerId) if managerState.peers.contains(peerId) =>
      val peer = managerState.peers(peerId)
      peer.ref ! PeerActor.SendMessage(message)

    case Terminated(ref) =>
      val maybePeerId = managerState.findTerminatedPeer(ref)
      maybePeerId.foreach { peerId =>
        peerEventBus ! Publish(PeerDisconnected(peerId))
        managerState.removeTerminatedPeer(peerId)
      }

    case IncomingConnectionHandshakeSuccess(peerId, peer) =>
      if (managerState.incomingPeers.size >= peerConfiguration.maxIncomingPeers) {
        managerState.removePendingIncomingPeer(peerId)
        peer.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)
      } else {
        managerState.promotePendingToHandshaked(peerId, peer)
      }
  }

  def createPeer(addr: InetSocketAddress, incomingConnection: Boolean): Peer = {
    val ref = peerFactory(context, addr, incomingConnection)
    context watch ref
    val peer = Peer(addr, ref, incomingConnection)
    managerState.addPeer(peer.id, peer)
    peer
  }

  def getPeers(): Future[Peers] = {
    implicit val timeout = Timeout(2.seconds)

    Future.traverse(managerState.peers.values) { peer =>
      (peer.ref ? PeerActor.GetStatus)
        .mapTo[PeerActor.StatusResponse]
        .map { sr => Success((peer, sr.status)) }
        .recover { case ex => Failure(ex) }
    }.map(r => Peers.apply(r.collect { case Success(v) => v }.toMap))
  }

}

object PeerManagerActor {
  def props[R <: HandshakeResult](peerDiscoveryManager: ActorRef,
                                  peerConfiguration: PeerConfiguration,
                                  peerMessageBus: ActorRef,
                                  knownNodesManager: ActorRef,
                                  handshaker: Handshaker[R],
                                  authHandshaker: AuthHandshaker,
                                  messageDecoder: MessageDecoder): Props =
    Props(new PeerManagerActor(peerMessageBus, peerDiscoveryManager, peerConfiguration,
      knownNodesManager = knownNodesManager,
      peerFactory = peerFactory(peerConfiguration, peerMessageBus, knownNodesManager, handshaker, authHandshaker, messageDecoder)))

  def peerFactory[R <: HandshakeResult](peerConfiguration: PeerConfiguration,
                                        peerEventBus: ActorRef,
                                        knownNodesManager: ActorRef,
                                        handshaker: Handshaker[R],
                                        authHandshaker: AuthHandshaker,
                                        messageDecoder: MessageDecoder): (ActorContext, InetSocketAddress, Boolean) => ActorRef = {
    (ctx, addr, incomingConnection) =>
      val id = addr.toString.filterNot(_ == '/')
      ctx.actorOf(PeerActor.props(addr, peerConfiguration, peerEventBus,
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
    val maxPendingPeers: Int
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

  class PeerManagerState(var pendingIncomingPeers: Map[PeerId, Peer], var peers: Map[PeerId, Peer]) {
    def incomingPeers: Map[PeerId, Peer] = peers.filter { case (_, p) => p.incomingConnection }
    def outgoingPeers: Map[PeerId, Peer] = peers.filter { case (_, p) => !p.incomingConnection }

    def removePendingIncomingPeer(peerId: PeerId): Unit =
      pendingIncomingPeers -= peerId

    def addPeer(peerId: PeerId, peer: Peer): Unit = {
      if (peer.incomingConnection)
        pendingIncomingPeers += peer.id -> peer
      else
        peers += peer.id -> peer
    }

    def promotePendingToHandshaked(peerId: PeerId, peer: Peer): Unit = {
      pendingIncomingPeers -= peerId
      peers += peerId -> peer
    }

    def findTerminatedPeer(ref: ActorRef): Iterable[PeerId] =
      (peers ++ pendingIncomingPeers).collect { case (id, Peer(_, peerRef, _)) if peerRef == ref => id }

    def removeTerminatedPeer(peerId: PeerId): Unit = {
      peers -= peerId
      pendingIncomingPeers -= peerId
    }

    def isConnectionHandled(addr: InetSocketAddress): Boolean =
      (peers ++ pendingIncomingPeers).values.map(_.remoteAddress).toSet.contains(addr)

  }

}
