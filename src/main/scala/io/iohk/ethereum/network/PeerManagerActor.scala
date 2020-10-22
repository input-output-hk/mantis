package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.Timeout
import io.iohk.ethereum.blockchain.sync.BlacklistSupport
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.network.PeerActor.PeerClosedConnection
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, PeerDiscoveryManager}
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
    discoveryConfig: DiscoveryConfig,
    externalSchedulerOpt: Option[Scheduler] = None
) extends Actor
    with ActorLogging
    with Stash
    with BlacklistSupport {

  /**
    * Maximum number of blacklisted nodes will never be larger than number of peers provided by discovery
    * Discovery provides remote nodes from all networks (ETC,ETH, Mordor etc.) only during handshake we learn that some
    * of the remote nodes are not compatible that's why we mark them as useless (blacklist them)
    */
  override val maxBlacklistedNodes: Int = discoveryConfig.nodesLimit

  import PeerManagerActor._
  import akka.pattern.{ask, pipe}

  private type PeerMap = Map[PeerId, Peer]

  // Subscribe to the handshake event of any peer
  peerEventBus ! Subscribe(SubscriptionClassifier.PeerHandshaked)

  def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() { case _ =>
      Stop
    }

  override def receive: Receive = start(Map.empty, Map.empty)

  def start(pendingPeers: PeerMap, peers: PeerMap): Receive = { case StartConnecting =>
    scheduleNodesUpdate()
    knownNodesManager ! KnownNodesManager.GetKnownNodes
    context become listen(pendingPeers, peers)
    unstashAll()
  }

  private def scheduleNodesUpdate(): Unit = {
    scheduler.scheduleWithFixedDelay(
      peerConfiguration.updateNodesInitialDelay,
      peerConfiguration.updateNodesInterval,
      peerDiscoveryManager,
      PeerDiscoveryManager.GetDiscoveredNodesInfo
    )
  }

  def listen(pendingPeers: PeerMap, peers: PeerMap): Receive = {
    handleCommonMessages(pendingPeers, peers) orElse
      handleBlacklistMessages orElse
      connections(pendingPeers, peers) orElse
      nodes(pendingPeers, peers) orElse { case _ =>
        stash()
      }
  }

  def nodes(pendingPeers: PeerMap, peers: PeerMap): Receive = {
    case KnownNodesManager.KnownNodes(nodes) =>
      val nodesToConnect = nodes.take(peerConfiguration.maxOutgoingPeers)

      if (nodesToConnect.nonEmpty) {
        log.debug("Trying to connect to {} known nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n))
      } else {
        log.debug("The known nodes list is empty")
      }

    case PeerDiscoveryManager.DiscoveredNodesInfo(nodesInfo) =>
      val peerAddresses = outgoingPeersAddresses(peers)

      val nodesToConnect = nodesInfo
        .filterNot { discoveryNodeInfo =>
          val socketAddress = discoveryNodeInfo.node.tcpSocketAddress
          peerAddresses.contains(socketAddress) || isBlacklisted(PeerAddress(socketAddress.getHostString))
        } // not already connected to or blacklisted
        .take(peerConfiguration.maxOutgoingPeers - peerAddresses.size)

      NetworkMetrics.DiscoveredPeersSize.set(nodesInfo.size)
      NetworkMetrics.BlacklistedPeersSize.set(blacklistedPeers.size)
      NetworkMetrics.PendingPeersSize.set(pendingPeers.size)

      log.info(
        s"Discovered ${nodesInfo.size} nodes, " +
          s"Blacklisted ${blacklistedPeers.size} nodes, " +
          s"connected to ${peers.size}/${peerConfiguration.maxOutgoingPeers + peerConfiguration.maxIncomingPeers}. " +
          s"Trying to connect to ${nodesToConnect.size} more nodes."
      )

      if (nodesToConnect.nonEmpty) {
        log.debug("Trying to connect to {} nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n.node.toUri))
      } else {
        log.debug("The nodes list is empty, no new nodes to connect to")
      }
  }

  def connections(pendingPeers: PeerMap, peers: PeerMap): Receive = {
    case PeerClosedConnection(peerAddress, reason) =>
      blacklist(
        PeerAddress(peerAddress),
        getBlacklistDuration(reason),
        s"peer disconnected due to: ${Disconnect.reasonToString(reason)}"
      )

    case HandlePeerConnection(connection, remoteAddress) =>
      handleConnection(connection, remoteAddress, pendingPeers, peers)

    case ConnectToPeer(uri) =>
      connectWith(uri, pendingPeers, peers)
  }

  def getBlacklistDuration(reason: Long): FiniteDuration = {
    import Disconnect.Reasons._
    reason match {
      case TooManyPeers => peerConfiguration.shortBlacklistDuration
      case _ => peerConfiguration.longBlacklistDuration
    }
  }

  private def handleConnection(
      connection: ActorRef,
      remoteAddress: InetSocketAddress,
      pendingPeers: PeerMap,
      peers: PeerMap
  ): Unit = {
    val connectionHandled = isConnectionHandled(remoteAddress, pendingPeers, peers)
    val isPendingPeersNotMaxValue = pendingPeers.size < peerConfiguration.maxPendingPeers

    val validConnection = for {
      validHandler <- validateConnection(
        remoteAddress,
        IncomingConnectionAlreadyHandled(remoteAddress, connection),
        connectionHandled
      )
      validNumber <- validateConnection(
        validHandler,
        MaxIncomingPendingConnections(connection),
        isPendingPeersNotMaxValue
      )
    } yield validNumber

    validConnection match {
      case Right(address) =>
        val (peer, newPendingPeers, newPeers) = createPeer(address, incomingConnection = true, pendingPeers, peers)
        peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
        context become listen(newPendingPeers, newPeers)
      case Left(error) => handleConnectionErrors(error)
    }
  }

  private def connectWith(uri: URI, pendingPeers: PeerMap, peers: PeerMap): Unit = {
    val remoteAddress = new InetSocketAddress(uri.getHost, uri.getPort)

    val connectionHandled = isConnectionHandled(remoteAddress, pendingPeers, peers)
    val isOutgoingPeersNotMaxValue = countOutgoingPeers(peers) < peerConfiguration.maxOutgoingPeers

    val validConnection = for {
      validHandler <- validateConnection(remoteAddress, OutgoingConnectionAlreadyHandled(uri), connectionHandled)
      validNumber <- validateConnection(validHandler, MaxOutgoingConnections, isOutgoingPeersNotMaxValue)
    } yield validNumber

    validConnection match {
      case Right(address) =>
        val (peer, newPendingPeers, newPeers) = createPeer(address, incomingConnection = false, pendingPeers, peers)
        peer.ref ! PeerActor.ConnectTo(uri)
        context become listen(newPendingPeers, newPeers)

      case Left(error) => handleConnectionErrors(error)
    }
  }

  private def isConnectionHandled(remoteAddress: InetSocketAddress, pendingPeers: PeerMap, peers: PeerMap): Boolean =
    !(peers ++ pendingPeers).values.map(_.remoteAddress).toSet.contains(remoteAddress)

  private def outgoingPeersAddresses(peers: PeerMap): Set[InetSocketAddress] =
    peers.filter { case (_, p) => !p.incomingConnection }.values.map(_.remoteAddress).toSet

  private def countOutgoingPeers(peers: PeerMap): Int = peers.count { case (_, p) => !p.incomingConnection }

  private def countIncomingPeers(peers: PeerMap): Int = peers.count { case (_, p) => p.incomingConnection }

  def handleCommonMessages(pendingPeers: PeerMap, peers: PeerMap): Receive = {
    case GetPeers =>
      getPeers(peers.values.toSet).pipeTo(sender())

    case SendMessage(message, peerId) if peers.contains(peerId) =>
      peers(peerId).ref ! PeerActor.SendMessage(message)

    case Terminated(ref) =>
      val terminatedPeers = terminatedPeersIds(ref, pendingPeers, peers)

      terminatedPeers.foreach { peerId =>
        peerEventBus ! Publish(PeerEvent.PeerDisconnected(peerId))
      }
      context unwatch ref
      context become listen(pendingPeers -- terminatedPeers, peers -- terminatedPeers)

    case PeerEvent.PeerHandshakeSuccessful(peer, _) if peer.incomingConnection =>
      if (countIncomingPeers(peers) >= peerConfiguration.maxIncomingPeers) {
        peer.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)
      } else {
        context become listen(pendingPeers - peer.id, peers + (peer.id -> peer))
      }
  }

  private def terminatedPeersIds(ref: ActorRef, pendingPeers: PeerMap, peers: PeerMap): Iterable[PeerId] = {
    (peers ++ pendingPeers).collect { case (id, Peer(_, peerRef, _)) if peerRef == ref => id }
  }

  private def createPeer(
      address: InetSocketAddress,
      incomingConnection: Boolean,
      pendingPeers: PeerMap,
      peers: PeerMap
  ): (Peer, PeerMap, PeerMap) = {
    val ref = peerFactory(context, address, incomingConnection)
    context watch ref
    val peer = Peer(address, ref, incomingConnection)
    val newPeer = peer.id -> peer

    val (newPendingPeers, newPeers) = if (peer.incomingConnection) {
      (pendingPeers + newPeer, peers)
    } else {
      (pendingPeers, peers + newPeer)
    }

    (peer, newPendingPeers, newPeers)
  }

  private def getPeers(peers: Set[Peer]): Future[Peers] = {
    implicit val timeout: Timeout = Timeout(2.seconds)

    Future
      .traverse(peers) { peer =>
        (peer.ref ? PeerActor.GetStatus)
          .mapTo[PeerActor.StatusResponse]
          .map { sr => Success((peer, sr.status)) }
          .recover { case ex => Failure(ex) }
      }
      .map(r => Peers.apply(r.collect { case Success(v) => v }.toMap))
  }

  private def validateConnection(
      remoteAddress: InetSocketAddress,
      error: ConnectionError,
      stateCondition: Boolean
  ): Either[ConnectionError, InetSocketAddress] = {
    Either.cond(stateCondition, remoteAddress, error)
  }

  private def handleConnectionErrors(error: ConnectionError): Unit = error match {
    case MaxIncomingPendingConnections(connection) =>
      log.debug("Maximum number of pending incoming peers reached")
      connection ! PoisonPill

    case IncomingConnectionAlreadyHandled(remoteAddress, connection) =>
      log.debug("Another connection with {} is already opened. Disconnecting", remoteAddress)
      connection ! PoisonPill

    case MaxOutgoingConnections =>
      log.debug("Maximum number of connected peers reached")

    case OutgoingConnectionAlreadyHandled(uri) =>
      log.debug("Another connection with {} is already opened", uri)
  }
}

object PeerManagerActor {
  def props[R <: HandshakeResult](
      peerDiscoveryManager: ActorRef,
      peerConfiguration: PeerConfiguration,
      peerMessageBus: ActorRef,
      knownNodesManager: ActorRef,
      handshaker: Handshaker[R],
      authHandshaker: AuthHandshaker,
      messageDecoder: MessageDecoder,
      discoveryConfig: DiscoveryConfig
  ): Props = {
    val factory: (ActorContext, InetSocketAddress, Boolean) => ActorRef =
      peerFactory(peerConfiguration, peerMessageBus, knownNodesManager, handshaker, authHandshaker, messageDecoder)

    Props(
      new PeerManagerActor(
        peerMessageBus,
        peerDiscoveryManager,
        peerConfiguration,
        knownNodesManager,
        peerFactory = factory,
        discoveryConfig
      )
    )
  }

  def peerFactory[R <: HandshakeResult](
      config: PeerConfiguration,
      eventBus: ActorRef,
      knownNodesManager: ActorRef,
      handshaker: Handshaker[R],
      authHandshaker: AuthHandshaker,
      messageDecoder: MessageDecoder
  ): (ActorContext, InetSocketAddress, Boolean) => ActorRef = { (ctx, address, incomingConnection) =>
    val id: String = address.toString.filterNot(_ == '/')
    val props = PeerActor.props(
      address,
      config,
      eventBus,
      knownNodesManager,
      incomingConnection,
      handshaker,
      authHandshaker,
      messageDecoder
    )
    ctx.actorOf(props, id)
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
    val maxOutgoingPeers: Int
    val maxIncomingPeers: Int
    val maxPendingPeers: Int
    val networkId: Int
    val updateNodesInitialDelay: FiniteDuration
    val updateNodesInterval: FiniteDuration
    val shortBlacklistDuration: FiniteDuration
    val longBlacklistDuration: FiniteDuration
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
    def handshaked: Seq[Peer] = peers.collect { case (peer, Handshaked) => peer }.toSeq
  }

  case class SendMessage(message: MessageSerializable, peerId: PeerId)

  sealed abstract class ConnectionError

  case class MaxIncomingPendingConnections(connection: ActorRef) extends ConnectionError

  case class IncomingConnectionAlreadyHandled(address: InetSocketAddress, connection: ActorRef) extends ConnectionError

  case object MaxOutgoingConnections extends ConnectionError

  case class OutgoingConnectionAlreadyHandled(uri: URI) extends ConnectionError

  case class PeerAddress(value: String) extends BlackListId

}
