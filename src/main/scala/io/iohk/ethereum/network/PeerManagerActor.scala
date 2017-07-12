package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.util.Timeout
import io.iohk.ethereum.network.PeerActor.Status.{Handshaked, Handshaking}
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
import io.iohk.ethereum.network.p2p.MessageSerializable
import io.iohk.ethereum.utils.NodeStatus

import scala.util.{Failure, Success}

class PeerManagerActor(
    peerEventBus: ActorRef,
    peerDiscoveryManager: ActorRef,
    peerConfiguration: PeerConfiguration,
    peerFactory: (ActorContext, InetSocketAddress) => ActorRef,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor with ActorLogging with Stash {

  import akka.pattern.{ask, pipe}
  import PeerManagerActor._

  var peers: Map[PeerId, Peer] = Map.empty

  private def scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  scheduler.schedule(5.seconds, 20.seconds) {
    peerDiscoveryManager ! PeerDiscoveryManager.GetDiscoveredNodes
  }

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  override def receive: Receive = {

    case StartConnecting =>
      context become listening
      unstashAll()

    case _ =>
      stash()

  }

  def listening: Receive = handleCommonMessages orElse {
    case msg: HandlePeerConnection =>
      context become(tryingToConnect, false)
      tryDiscardPeersToFreeUpLimit()
        .map(_ => (msg, Success(())))
        .recover { case ex => (msg, Failure(ex)) }
        .pipeTo(self)

    case msg: ConnectToPeer =>
      context become(tryingToConnect, false)
      tryDiscardPeersToFreeUpLimit()
        .map(_ => (msg, Success(())))
        .recover { case ex => (msg, Failure(ex)) }
        .pipeTo(self)

    case PeerDiscoveryManager.DiscoveredNodes(nodes) =>
      val peerAddresses = peers.values.map(_.remoteAddress).toSet

      val nodesToConnect = nodes
        .filterNot(n => peerAddresses.contains(n.addr)) // not already connected to
        .toSeq
        .sortBy(-_.addTimestamp)
        .take(peerConfiguration.maxPeers - peerAddresses.size)

      if (nodesToConnect.nonEmpty) {
        log.info("Trying to connect to {} nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n.toUri))
      }
  }

  def handleCommonMessages: Receive = {
    case GetPeers =>
      getPeers().pipeTo(sender())

    case SendMessage(message, peerId) if peers.contains(peerId) =>
      val peer = peers(peerId)
      peer.ref ! PeerActor.SendMessage(message)

    case Terminated(ref) =>
      val maybePeerId = peers.collect{ case (id, Peer(_, peerRef)) if peerRef == ref => id }
      maybePeerId.foreach{ peerId =>
        peerEventBus ! Publish(PeerDisconnected(peerId))
        peers -= peerId
      }
  }

  def tryingToConnect: Receive = handleCommonMessages orElse {
    case _: HandlePeerConnection | _: ConnectToPeer | PeerDiscoveryManager.DiscoveredNodes =>
      stash()

    case (HandlePeerConnection(connection, remoteAddress), Success(_)) =>
      context.unbecome()
      val peer = createPeer(remoteAddress)
      peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
      unstashAll()

    case (HandlePeerConnection(connection, remoteAddress), Failure(_)) =>
      log.info("Maximum number of connected peers reached. Peer {} will be disconnected.", remoteAddress)
      context.unbecome()
      val peer = createPeer(remoteAddress)
      peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
      peer.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)
      unstashAll()

    case (ConnectToPeer(uri), Success(_)) =>
      context.unbecome()
      val peer = createPeer(new InetSocketAddress(uri.getHost, uri.getPort))
      peer.ref ! PeerActor.ConnectTo(uri)
      unstashAll()

    case (ConnectToPeer(uri), Failure(_)) =>
      log.info("Maximum number of connected peers reached. Not connecting to {}", uri)
      context.unbecome()
      unstashAll()
  }

  def createPeer(addr: InetSocketAddress): Peer = {
    val ref = peerFactory(context, addr)
    context watch ref
    val peer = Peer(addr, ref)
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

  def tryDiscardPeersToFreeUpLimit(): Future[Unit] = {
    getPeers() flatMap { peers =>
      val peersToPotentiallyDisconnect = peers.peers.filter {
        case (_, Handshaking(numRetries)) if numRetries > 0 => true /* still handshaking and retried at least once */
        case (_, PeerActor.Status.Disconnected) => true /* already disconnected */
        case _ => false
      }

      if (peers.peers.size - peersToPotentiallyDisconnect.size >= peerConfiguration.maxPeers) {
        Future.failed(new RuntimeException("Too many peers"))
      } else {
        val numPeersToDisconnect = (peers.peers.size + 1 - peerConfiguration.maxPeers) max 0

        val peersToDisconnect = peersToPotentiallyDisconnect.toSeq.sortBy {
          case (_, PeerActor.Status.Disconnected) => 0
          case (_, _: Handshaking) => 1
          case _ => 2
        }.take(numPeersToDisconnect)

        peersToDisconnect.foreach { case (p, _) => p.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers) }
        Future.successful(())
      }
    }
  }

}

object PeerManagerActor {
  def props[R <: HandshakeResult](nodeStatusHolder: Agent[NodeStatus],
                                  peerConfiguration: PeerConfiguration,
                                  peerMessageBus: ActorRef,
                                  peerDiscoveryManager: ActorRef,
                                  handshaker: Handshaker[R]): Props =
    Props(new PeerManagerActor(peerMessageBus, peerDiscoveryManager, peerConfiguration,
      peerFactory = peerFactory(nodeStatusHolder, peerConfiguration, peerMessageBus, handshaker)))

  def peerFactory[R <: HandshakeResult](nodeStatusHolder: Agent[NodeStatus],
                                        peerConfiguration: PeerConfiguration,
                                        peerEventBus: ActorRef,
                                        handshaker: Handshaker[R]): (ActorContext, InetSocketAddress) => ActorRef = {
    (ctx, addr) =>
      val id = addr.toString.filterNot(_ == '/')
      ctx.actorOf(PeerActor.props(addr, nodeStatusHolder, peerConfiguration, peerEventBus,
        handshaker), id)
  }

  trait PeerConfiguration {
    val connectRetryDelay: FiniteDuration
    val connectMaxRetries: Int
    val disconnectPoisonPillTimeout: FiniteDuration
    val waitForHelloTimeout: FiniteDuration
    val waitForStatusTimeout: FiniteDuration
    val waitForChainCheckTimeout: FiniteDuration
    val fastSyncHostConfiguration: FastSyncHostConfiguration
    val maxPeers: Int
    val networkId: Int
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
