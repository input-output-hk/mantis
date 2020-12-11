package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.util.{ByteString, Timeout}
import io.iohk.ethereum.blockchain.sync.BlacklistSupport
import io.iohk.ethereum.blockchain.sync.BlacklistSupport.BlackListId
import io.iohk.ethereum.network.PeerActor.PeerClosedConnection
import io.iohk.ethereum.network.PeerActor.Status.Handshaked
import io.iohk.ethereum.network.PeerEventBusActor._
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.discovery.{DiscoveryConfig, Node, PeerDiscoveryManager}
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeResult
import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.network.p2p.{MessageDecoder, MessageSerializable}
import io.iohk.ethereum.network.rlpx.AuthHandshaker
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import monix.eval.Task
import monix.execution.{Scheduler => MonixScheduler}
import org.bouncycastle.util.encoders.Hex
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

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
    * of the remote nodes are not compatible that's why we mark them as useless (blacklist them).
    *
    * The number of nodes in the current discovery is unlimited, but a guide may be the size of the routing table:
    * one bucket for each bit in the hash of the public key, times the bucket size.
    */
  override val maxBlacklistedNodes: Int = 32 * 8 * discoveryConfig.kademliaBucketSize

  import PeerManagerActor._
  import akka.pattern.{ask, pipe}

  private type PeerMap = Map[PeerId, Peer]

  implicit class ConnectedPeersOps(connectedPeers: ConnectedPeers) {
    def outgoingConnectionDemand: Int =
      peerConfiguration.maxOutgoingPeers - connectedPeers.outgoingPeersCount

    def canConnectTo(node: Node): Boolean = {
      val socketAddress = node.tcpSocketAddress
      val alreadyConnected =
        connectedPeers.isConnectionHandled(socketAddress) ||
          connectedPeers.hasHandshakedWith(node.id)

      !alreadyConnected && !isBlacklisted(PeerAddress(socketAddress.getHostString))
    }
  }

  // Subscribe to the handshake event of any peer
  peerEventBus ! Subscribe(SubscriptionClassifier.PeerHandshaked)

  def scheduler: Scheduler = externalSchedulerOpt getOrElse context.system.scheduler
  implicit val monix: MonixScheduler = MonixScheduler(context.dispatcher)

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() { case _ =>
      Stop
    }

  override def receive: Receive = {
    case StartConnecting =>
      scheduleNodesUpdate()
      knownNodesManager ! KnownNodesManager.GetKnownNodes
      context become listening(ConnectedPeers.empty)
      unstashAll()
    case _ =>
      stash()
  }

  private def scheduleNodesUpdate(): Unit = {
    scheduler.scheduleWithFixedDelay(
      peerConfiguration.updateNodesInitialDelay,
      peerConfiguration.updateNodesInterval,
      peerDiscoveryManager,
      PeerDiscoveryManager.GetDiscoveredNodesInfo
    )
  }

  private def listening(connectedPeers: ConnectedPeers): Receive = {
    handleCommonMessages(connectedPeers) orElse
      handleBlacklistMessages orElse
      handleConnections(connectedPeers) orElse
      handleNewNodesToConnectMessages(connectedPeers)
  }

  private def handleNewNodesToConnectMessages(connectedPeers: ConnectedPeers): Receive = {
    case KnownNodesManager.KnownNodes(nodes) =>
      val nodesToConnect = nodes.take(peerConfiguration.maxOutgoingPeers)

      if (nodesToConnect.nonEmpty) {
        log.debug("Trying to connect to {} known nodes", nodesToConnect.size)
        nodesToConnect.foreach(n => self ! ConnectToPeer(n))
      } else {
        log.debug("The known nodes list is empty")
      }

    case PeerDiscoveryManager.RandomNodeInfo(node) =>
      maybeConnectToRandomNode(connectedPeers, node)

    case PeerDiscoveryManager.DiscoveredNodesInfo(nodes) =>
      maybeConnectToDiscoveredNodes(connectedPeers, nodes)
  }

  private def maybeConnectToRandomNode(connectedPeers: ConnectedPeers, node: Node): Unit = {
    if (connectedPeers.outgoingConnectionDemand > 0) {
      if (connectedPeers.canConnectTo(node)) {
        self ! ConnectToPeer(node.toUri)
      } else {
        peerDiscoveryManager ! PeerDiscoveryManager.GetRandomNodeInfo
      }
    }
  }

  private def maybeConnectToDiscoveredNodes(connectedPeers: ConnectedPeers, nodes: Set[Node]): Unit = {
    val nodesToConnect = nodes
      .filter(connectedPeers.canConnectTo)
      .take(connectedPeers.outgoingConnectionDemand)

    NetworkMetrics.DiscoveredPeersSize.set(nodes.size)
    NetworkMetrics.BlacklistedPeersSize.set(blacklistedPeers.size)
    NetworkMetrics.PendingPeersSize.set(connectedPeers.pendingPeersCount)

    log.info(
      s"Discovered ${nodes.size} nodes, " +
        s"Blacklisted ${blacklistedPeers.size} nodes, " +
        s"handshaked to ${connectedPeers.handshakedPeersCount}/${peerConfiguration.maxOutgoingPeers + peerConfiguration.maxIncomingPeers}, " +
        s"pending connection attempts ${connectedPeers.pendingPeersCount}. " +
        s"Trying to connect to ${nodesToConnect.size} more nodes."
    )

    if (nodesToConnect.nonEmpty) {
      log.debug("Trying to connect to {} nodes", nodesToConnect.size)
      nodesToConnect.foreach(n => self ! ConnectToPeer(n.toUri))
    } else {
      log.debug("The nodes list is empty, no new nodes to connect to")
    }

    // Make sure the background lookups keep going and we don't get stuck with 0
    // nodes to connect to until the next discovery scan loop. Only sending 1
    // request so we don't rack up too many pending futures, just trigger a a
    // search if needed.
    if (connectedPeers.outgoingConnectionDemand > nodesToConnect.size) {
      peerDiscoveryManager ! PeerDiscoveryManager.GetRandomNodeInfo
    }
  }

  private def handleConnections(connectedPeers: ConnectedPeers): Receive = {
    case PeerClosedConnection(peerAddress, reason) =>
      blacklist(
        PeerAddress(peerAddress),
        getBlacklistDuration(reason),
        s"peer disconnected due to: ${Disconnect.reasonToString(reason)}"
      )

    case HandlePeerConnection(connection, remoteAddress) =>
      handleConnection(connection, remoteAddress, connectedPeers)

    case ConnectToPeer(uri) =>
      connectWith(uri, connectedPeers)
  }

  private def getBlacklistDuration(reason: Long): FiniteDuration = {
    import Disconnect.Reasons._
    reason match {
      case TooManyPeers => peerConfiguration.shortBlacklistDuration
      case _ => peerConfiguration.longBlacklistDuration
    }
  }

  private def handleConnection(
      connection: ActorRef,
      remoteAddress: InetSocketAddress,
      connectedPeers: ConnectedPeers
  ): Unit = {
    val alreadyConnectedToPeer = connectedPeers.isConnectionHandled(remoteAddress)
    val isPendingPeersNotMaxValue = connectedPeers.incomingPendingPeersCount < peerConfiguration.maxPendingPeers

    val validConnection = for {
      validHandler <- validateConnection(
        remoteAddress,
        IncomingConnectionAlreadyHandled(remoteAddress, connection),
        !alreadyConnectedToPeer
      )
      validNumber <- validateConnection(
        validHandler,
        MaxIncomingPendingConnections(connection),
        isPendingPeersNotMaxValue
      )
    } yield validNumber

    validConnection match {
      case Right(address) =>
        val (peer, newConnectedPeers) = createPeer(address, incomingConnection = true, connectedPeers)
        peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
        context become listening(newConnectedPeers)

      case Left(error) =>
        handleConnectionErrors(error)
    }
  }

  private def connectWith(uri: URI, connectedPeers: ConnectedPeers): Unit = {
    val nodeId = ByteString(Hex.decode(uri.getUserInfo))
    val remoteAddress = new InetSocketAddress(uri.getHost, uri.getPort)

    val alreadyConnectedToPeer =
      connectedPeers.hasHandshakedWith(nodeId) || connectedPeers.isConnectionHandled(remoteAddress)
    val isOutgoingPeersNotMaxValue = connectedPeers.outgoingPeersCount < peerConfiguration.maxOutgoingPeers

    val validConnection = for {
      validHandler <- validateConnection(remoteAddress, OutgoingConnectionAlreadyHandled(uri), !alreadyConnectedToPeer)
      validNumber <- validateConnection(validHandler, MaxOutgoingConnections, isOutgoingPeersNotMaxValue)
    } yield validNumber

    validConnection match {
      case Right(address) =>
        val (peer, newConnectedPeers) = createPeer(address, incomingConnection = false, connectedPeers)
        peer.ref ! PeerActor.ConnectTo(uri)
        context become listening(newConnectedPeers)

      case Left(error) => handleConnectionErrors(error)
    }
  }

  private def handleCommonMessages(connectedPeers: ConnectedPeers): Receive = {
    case GetPeers =>
      getPeers(connectedPeers.peers.values.toSet).runToFuture.pipeTo(sender())

    case SendMessage(message, peerId) if connectedPeers.getPeer(peerId).isDefined =>
      connectedPeers.getPeer(peerId).get.ref ! PeerActor.SendMessage(message)

    case Terminated(ref) =>
      val (terminatedPeersIds, newConnectedPeers) = connectedPeers.removeTerminatedPeer(ref)
      terminatedPeersIds.foreach { peerId =>
        peerEventBus ! Publish(PeerEvent.PeerDisconnected(peerId))
      }
      // Try to replace a lost connection with another one.
      if (newConnectedPeers.outgoingConnectionDemand > 0) {
        peerDiscoveryManager ! PeerDiscoveryManager.GetRandomNodeInfo
      }
      context unwatch ref
      context become listening(newConnectedPeers)

    case PeerEvent.PeerHandshakeSuccessful(handshakedPeer, _) =>
      if (
        handshakedPeer.incomingConnection && connectedPeers.incomingHandshakedPeersCount >= peerConfiguration.maxIncomingPeers
      ) {
        handshakedPeer.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)
      } else if (handshakedPeer.nodeId.exists(connectedPeers.hasHandshakedWith)) {
        // FIXME: peers received after handshake should always have their nodeId defined, we could maybe later distinguish
        //        it into PendingPeer/HandshakedPeer classes

        // Even though we do already validations for this, we might have missed it someone tried connecting to us at the
        // same time as we do
        log.debug(s"Disconnecting from ${handshakedPeer.remoteAddress} as we are already connected to him")
        handshakedPeer.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.AlreadyConnected)
      } else {
        context become listening(connectedPeers.promotePeerToHandshaked(handshakedPeer))
      }
  }

  private def createPeer(
      address: InetSocketAddress,
      incomingConnection: Boolean,
      connectedPeers: ConnectedPeers
  ): (Peer, ConnectedPeers) = {
    val ref = peerFactory(context, address, incomingConnection)
    context watch ref
    val pendingPeer = Peer(address, ref, incomingConnection, None)

    val newConnectedPeers = connectedPeers.addNewPendingPeer(pendingPeer)

    (pendingPeer, newConnectedPeers)
  }

  private def getPeers(peers: Set[Peer]): Task[Peers] = {
    Task
      .parSequence(peers.map(getPeerStatus))
      .map(_.collect { case Success(v) => v }.toMap)
      .map(Peers.apply)
  }

  private def getPeerStatus(peer: Peer): Task[Try[(Peer, PeerActor.Status)]] = {
    implicit val timeout: Timeout = Timeout(2.seconds)
      Task.fromFuture(
        (peer.ref ? PeerActor.GetStatus).mapTo[PeerActor.StatusResponse]
      )
      .map { sr => Success((peer, sr.status)) }
      .onErrorHandle(Failure(_))
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
  // scalastyle:off parameter.number
  def props[R <: HandshakeResult](
      peerDiscoveryManager: ActorRef,
      peerConfiguration: PeerConfiguration,
      peerMessageBus: ActorRef,
      knownNodesManager: ActorRef,
      handshaker: Handshaker[R],
      authHandshaker: AuthHandshaker,
      messageDecoder: MessageDecoder,
      discoveryConfig: DiscoveryConfig,
      bestProtocolVersion: Version
  ): Props = {
    val factory: (ActorContext, InetSocketAddress, Boolean) => ActorRef =
      peerFactory(
        peerConfiguration,
        peerMessageBus,
        knownNodesManager,
        handshaker,
        authHandshaker,
        messageDecoder,
        bestProtocolVersion
      )

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
  // scalastyle:on parameter.number

  def peerFactory[R <: HandshakeResult](
      config: PeerConfiguration,
      eventBus: ActorRef,
      knownNodesManager: ActorRef,
      handshaker: Handshaker[R],
      authHandshaker: AuthHandshaker,
      messageDecoder: MessageDecoder,
      bestProtocolVersion: Version
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
      messageDecoder,
      bestProtocolVersion
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
