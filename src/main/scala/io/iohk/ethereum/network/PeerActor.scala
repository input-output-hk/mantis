package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.SupervisorStrategy.Escalate
import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.network.PeerActor.Status._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerHandshakeSuccessful}
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeComplete.{HandshakeFailure, HandshakeSuccess}
import io.iohk.ethereum.network.handshaker.Handshaker.{HandshakeResult, NextMessage}
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.ProtocolNegotiator
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.rlpx.{AuthHandshaker, RLPxConnectionHandler}
import io.iohk.ethereum.utils.Logger
import org.bouncycastle.util.encoders.Hex

/**
  * Peer actor is responsible for initiating and handling high-level connection with peer.
  * It creates child RLPxConnectionActor for handling underlying RLPx communication.
  * Once RLPx connection is established it proceeds with protocol handshake (i.e `Hello`
  * and `Status` exchange).
  * Once that's done it can send/receive messages with peer (HandshakedHandler.receive).
  */
class PeerActor[R <: HandshakeResult](
    peerAddress: InetSocketAddress,
    rlpxConnectionFactory: ActorContext => ActorRef,
    val peerConfiguration: PeerConfiguration,
    peerEventBus: ActorRef,
    knownNodesManager: ActorRef,
    incomingConnection: Boolean,
    externalSchedulerOpt: Option[Scheduler] = None,
    initHandshaker: Handshaker[R]
) extends Actor
    with ActorLogging
    with Stash {

  import PeerActor._
  import context.{dispatcher, system}

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() { case _ =>
      Escalate
    }

  def scheduler: Scheduler = externalSchedulerOpt getOrElse system.scheduler

  val peerId: PeerId = PeerId.fromRef(self)

  override def receive: Receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = stashMessages orElse {
    case HandleConnection(connection, remoteAddress) =>
      val rlpxConnection = createRlpxConnection(remoteAddress, None)
      rlpxConnection.ref ! RLPxConnectionHandler.HandleConnection(connection)
      context become waitingForConnectionResult(rlpxConnection)

    case ConnectTo(uri) =>
      val rlpxConnection = createRlpxConnection(new InetSocketAddress(uri.getHost, uri.getPort), Some(uri))
      rlpxConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
      context become waitingForConnectionResult(rlpxConnection)

    case GetStatus => sender() ! StatusResponse(Idle)
  }

  def createRlpxConnection(remoteAddress: InetSocketAddress, uriOpt: Option[URI]): RLPxConnection = {
    val ref = rlpxConnectionFactory(context)
    context watch ref
    RLPxConnection(ref, remoteAddress, uriOpt)
  }

  private def modifyOutGoingUri(remoteNodeId: ByteString, rlpxConnection: RLPxConnection, uri: URI): URI = {
    val host = getHostName(rlpxConnection.remoteAddress.getAddress)
    val port = rlpxConnection.remoteAddress.getPort
    val query = Option(uri.getQuery).getOrElse(s"discport=$port")
    new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray)}@$host:$port?$query")
  }

  def waitingForConnectionResult(rlpxConnection: RLPxConnection, numRetries: Int = 0): Receive =
    handleTerminated(rlpxConnection, numRetries, Connecting) orElse stashMessages orElse {
      case RLPxConnectionHandler.ConnectionEstablished(remoteNodeId) =>
        val newUri =
          rlpxConnection.uriOpt.map(outGoingUri => modifyOutGoingUri(remoteNodeId, rlpxConnection, outGoingUri))
        processHandshakerNextMessage(initHandshaker, remoteNodeId, rlpxConnection.copy(uriOpt = newUri), numRetries)

      case RLPxConnectionHandler.ConnectionFailed =>
        log.debug("Failed to establish RLPx connection")
        rlpxConnection.uriOpt match {
          case Some(uri) if numRetries < peerConfiguration.connectMaxRetries =>
            context unwatch rlpxConnection.ref
            scheduleConnectRetry(uri, numRetries)
          case Some(uri) =>
            context.parent ! PeerClosedConnection(peerAddress.getHostString, Disconnect.Reasons.Other)
            knownNodesManager ! KnownNodesManager.RemoveKnownNode(uri)
            context stop self
          case None =>
            log.debug("Connection was initiated by remote peer, not attempting to reconnect")
            context stop self
        }

      case GetStatus => sender() ! StatusResponse(Connecting)
    }

  def processingHandshaking(
      handshaker: Handshaker[R],
      remoteNodeId: ByteString,
      rlpxConnection: RLPxConnection,
      timeout: Cancellable,
      numRetries: Int
  ): Receive =
    handleTerminated(rlpxConnection, numRetries, Handshaking(numRetries)) orElse
      handleDisconnectMsg(rlpxConnection, Handshaking(numRetries)) orElse
      handlePingMsg(rlpxConnection) orElse stashMessages orElse {

        case RLPxConnectionHandler.MessageReceived(msg) =>
          // Processes the received message, cancels the timeout and processes a new message but only if the handshaker
          // handles the received message
          handshaker.applyMessage(msg).foreach { newHandshaker =>
            timeout.cancel()
            processHandshakerNextMessage(newHandshaker, remoteNodeId, rlpxConnection, numRetries)
          }
          handshaker.respondToRequest(msg).foreach(msgToSend => rlpxConnection.sendMessage(msgToSend))

        case ResponseTimeout =>
          timeout.cancel()
          val newHandshaker = handshaker.processTimeout
          processHandshakerNextMessage(newHandshaker, remoteNodeId, rlpxConnection, numRetries)

        case GetStatus => sender() ! StatusResponse(Handshaking(numRetries))

      }

  /**
    * Asks for the next message to send to the handshaker, or, if there is None,
    * becomes MessageHandler if handshake was successful or disconnects from the peer otherwise
    *
    * @param handshaker
    * @param rlpxConnection
    * @param numRetries , number of connection retries done during RLPxConnection establishment
    */
  private def processHandshakerNextMessage(
      handshaker: Handshaker[R],
      remoteNodeId: ByteString,
      rlpxConnection: RLPxConnection,
      numRetries: Int
  ): Unit =
    handshaker.nextMessage match {
      case Right(NextMessage(msgToSend, timeoutTime)) =>
        rlpxConnection.sendMessage(msgToSend)
        val newTimeout = scheduler.scheduleOnce(timeoutTime, self, ResponseTimeout)
        context become processingHandshaking(handshaker, remoteNodeId, rlpxConnection, newTimeout, numRetries)

      case Left(HandshakeSuccess(handshakeResult)) =>
        rlpxConnection.uriOpt.foreach { uri => knownNodesManager ! KnownNodesManager.AddKnownNode(uri) }
        context become new HandshakedPeer(remoteNodeId, rlpxConnection, handshakeResult).receive
        unstashAll()

      case Left(HandshakeFailure(reason)) =>
        context.parent ! PeerClosedConnection(peerAddress.getHostString, reason)
        rlpxConnection.uriOpt.foreach { uri => knownNodesManager ! KnownNodesManager.RemoveKnownNode(uri) }
        disconnectFromPeer(rlpxConnection, reason)

    }

  private def scheduleConnectRetry(uri: URI, numRetries: Int): Unit = {
    log.debug("Scheduling connection retry in {}", peerConfiguration.connectRetryDelay)
    scheduler.scheduleOnce(peerConfiguration.connectRetryDelay, self, RetryConnectionTimeout)
    context become {
      case RetryConnectionTimeout => reconnect(uri, numRetries + 1)
      case GetStatus => sender() ! StatusResponse(Connecting)
    }
  }

  private def disconnectFromPeer(rlpxConnection: RLPxConnection, reason: Int): Unit = {
    rlpxConnection.sendMessage(Disconnect(reason))
    scheduler.scheduleOnce(peerConfiguration.disconnectPoisonPillTimeout, self, PoisonPill)
    context unwatch rlpxConnection.ref
    context become disconnected
  }

  private def stopActor(rlpxConnection: RLPxConnection, status: Status): Unit = status match {
    case Handshaked => gracefulStop(rlpxConnection)
    case _ => context stop self
  }

  private def gracefulStop(rlpxConnection: RLPxConnection): Unit = {
    scheduler.scheduleOnce(peerConfiguration.disconnectPoisonPillTimeout, self, PoisonPill)
    context unwatch rlpxConnection.ref
    context become disconnected
  }

  def disconnected: Receive = { case GetStatus =>
    sender() ! StatusResponse(Disconnected)
  }

  def handleTerminated(rlpxConnection: RLPxConnection, numRetries: Int, status: Status): Receive = {
    case Terminated(actor) if actor == rlpxConnection.ref =>
      log.debug(s"Underlying rlpx connection with peer $peerId closed")
      rlpxConnection.uriOpt match {
        case Some(uri) if numRetries < peerConfiguration.connectMaxRetries =>
          scheduleConnectRetry(uri, numRetries + 1)
        case Some(uri) =>
          context.parent ! PeerClosedConnection(peerAddress.getHostString, Disconnect.Reasons.Other)
          knownNodesManager ! KnownNodesManager.RemoveKnownNode(uri)
          stopActor(rlpxConnection, status)
        case None =>
          stopActor(rlpxConnection, status)
      }
  }

  def reconnect(uri: URI, numRetries: Int): Unit = {
    log.debug("Trying to reconnect")
    val address = new InetSocketAddress(uri.getHost, uri.getPort)
    val newConnection = createRlpxConnection(address, Some(uri))
    newConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
    context become waitingForConnectionResult(newConnection, numRetries)
  }

  def handlePingMsg(rlpxConnection: RLPxConnection): Receive = { case RLPxConnectionHandler.MessageReceived(_: Ping) =>
    rlpxConnection.sendMessage(Pong())
  }

  def handleDisconnectMsg(rlpxConnection: RLPxConnection, status: Status): Receive = {
    case RLPxConnectionHandler.MessageReceived(d: Disconnect) =>
      import Disconnect.Reasons._
      d.reason match {
        case IncompatibleP2pProtocolVersion | UselessPeer | NullNodeIdentityReceived | UnexpectedIdentity |
            IdentityTheSame | Other =>
          context.parent ! PeerClosedConnection(peerAddress.getHostString, d.reason)
          rlpxConnection.uriOpt.foreach(uri => knownNodesManager ! KnownNodesManager.RemoveKnownNode(uri))
        case TooManyPeers =>
          context.parent ! PeerClosedConnection(peerAddress.getHostString, d.reason)
        case _ => // nothing
      }
      log.debug(s"Received {}. Closing connection with peer ${peerAddress.getHostString}:${peerAddress.getPort}", d)
      stopActor(rlpxConnection, status)
  }

  def stashMessages: Receive = { case _: SendMessage | _: DisconnectPeer =>
    stash()
  }

  // The actor logs incoming messages, which can be quite verbose even for DEBUG mode.
  // ActorLogging doesn't support TRACE, but we can push more details if trace is enabled using the normal logging facilites.
  object MessageLogger extends Logger {
    val isTraceEnabled = {
      var enabled = false
      log.whenTraceEnabled({ enabled = true })
      enabled
    }
    def logMessage(peerId: PeerId, message: Message): Unit =
      // Sometimes potentially seeing the full block in the result is useful.
      if (isTraceEnabled) {
        log.trace(s"Received message: {} from $peerId", message)
      } else {
        log.debug(s"Received message: {} from $peerId", message.toShortString)
      }
  }

  class HandshakedPeer(remoteNodeId: ByteString, rlpxConnection: RLPxConnection, handshakeResult: R) {

    val peer: Peer = Peer(peerAddress, self, incomingConnection, Some(remoteNodeId))
    peerEventBus ! Publish(PeerHandshakeSuccessful(peer, handshakeResult))

    /**
      * main behavior of actor that handles peer communication and subscriptions for messages
      */
    def receive: Receive =
      handlePingMsg(rlpxConnection) orElse
        handleDisconnectMsg(rlpxConnection, Handshaked) orElse
        handleTerminated(rlpxConnection, 0, Handshaked) orElse {

          case RLPxConnectionHandler.MessageReceived(message) =>
            MessageLogger.logMessage(peerId, message)
            peerEventBus ! Publish(MessageFromPeer(message, peer.id))

          case DisconnectPeer(reason) =>
            disconnectFromPeer(rlpxConnection, reason)

          case SendMessage(message) =>
            rlpxConnection.sendMessage(message)

          case GetStatus =>
            sender() ! StatusResponse(Handshaked)

        }
  }

}

object PeerActor {
  // scalastyle:off parameter.number
  def props[R <: HandshakeResult](
      peerAddress: InetSocketAddress,
      peerConfiguration: PeerConfiguration,
      peerEventBus: ActorRef,
      knownNodesManager: ActorRef,
      incomingConnection: Boolean,
      handshaker: Handshaker[R],
      authHandshaker: AuthHandshaker,
      messageDecoder: MessageDecoder,
      protocolNegotiator: ProtocolNegotiator
  ): Props =
    Props(
      new PeerActor(
        peerAddress,
        rlpxConnectionFactory(authHandshaker, messageDecoder, peerConfiguration.rlpxConfiguration, protocolNegotiator),
        peerConfiguration,
        peerEventBus,
        knownNodesManager,
        incomingConnection,
        initHandshaker = handshaker
      )
    )
  // scalastyle:on parameter.number

  def rlpxConnectionFactory(
      authHandshaker: AuthHandshaker,
      messageDecoder: MessageDecoder,
      rlpxConfiguration: RLPxConfiguration,
      protocolNegotiator: ProtocolNegotiator
  ): ActorContext => ActorRef = { ctx =>
    ctx.actorOf(
      RLPxConnectionHandler
        .props(NetworkMessageDecoder orElse messageDecoder, protocolNegotiator, authHandshaker, rlpxConfiguration),
      "rlpx-connection"
    )
  }

  case class RLPxConnection(ref: ActorRef, remoteAddress: InetSocketAddress, uriOpt: Option[URI]) {
    def sendMessage(message: MessageSerializable): Unit = {
      ref ! RLPxConnectionHandler.SendMessage(message)
    }
  }

  case class HandleConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class IncomingConnectionHandshakeSuccess(peer: Peer)

  case class ConnectTo(uri: URI)

  case class SendMessage(message: MessageSerializable)

  case class PeerClosedConnection(peerHostAddress: String, reason: Long)

  private case object RetryConnectionTimeout

  private case object ResponseTimeout

  case object GetStatus

  case class StatusResponse(status: Status)

  case class DisconnectPeer(reason: Int)

  sealed trait Status

  object Status {

    case object Idle extends Status

    case object Connecting extends Status

    case class Handshaking(numRetries: Int) extends Status

    case object Handshaked extends Status

    case object Disconnected extends Status

  }

}
