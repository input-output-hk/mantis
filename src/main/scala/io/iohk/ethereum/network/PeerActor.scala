package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.SupervisorStrategy.Stop
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import akka.actor._
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.messages.Versions
import io.iohk.ethereum.network.rlpx.{AuthHandshaker, RLPxConnectionHandler}
import io.iohk.ethereum.network.PeerActor.Status._
import io.iohk.ethereum.network.PeerEventBusActor.PeerEvent.{MessageFromPeer, PeerHandshakeSuccessful}
import io.iohk.ethereum.network.PeerEventBusActor.Publish
import io.iohk.ethereum.utils.NodeStatus
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeComplete.{HandshakeFailure, HandshakeSuccess}
import io.iohk.ethereum.network.handshaker.Handshaker.{HandshakeResult, NextMessage}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.util.encoders.Hex


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
    initHandshaker: Handshaker[R])
  extends Actor with ActorLogging with Stash {

  import PeerActor._
  import context.{dispatcher, system}

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  def scheduler: Scheduler = externalSchedulerOpt getOrElse system.scheduler

  val peerId: PeerId = PeerId(self.path.name)

  val peer: Peer = Peer(peerAddress, self, incomingConnection)

  override def receive: Receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = stashMessages orElse {
    case HandleConnection(connection, remoteAddress) =>
      val rlpxConnection = createRlpxConnection(remoteAddress, None, false)
      rlpxConnection.ref ! RLPxConnectionHandler.HandleConnection(connection)
      context become waitingForConnectionResult(rlpxConnection)

    case ConnectTo(uri) =>
      val rlpxConnection = createRlpxConnection(new InetSocketAddress(uri.getHost, uri.getPort), Some(uri), true)
      rlpxConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
      context become waitingForConnectionResult(rlpxConnection)

    case GetStatus => sender() ! StatusResponse(Idle)
  }

  def createRlpxConnection(remoteAddress: InetSocketAddress, uriOpt: Option[URI], isInitiator: Boolean): RLPxConnection = {
    val ref = rlpxConnectionFactory(context)
    context watch ref
    RLPxConnection(ref, remoteAddress, uriOpt, isInitiator)
  }

  def waitingForConnectionResult(rlpxConnection: RLPxConnection, numRetries: Int = 0): Receive =
    handleTerminated(rlpxConnection) orElse stashMessages orElse {
      case RLPxConnectionHandler.ConnectionEstablished(remoteNodeId) =>
        val uri =
          new URI(s"enode://${Hex.toHexString(remoteNodeId.toArray)}@${rlpxConnection.remoteAddress.getHostName}:${rlpxConnection.remoteAddress.getPort}")
        processHandshakerNextMessage(initHandshaker, rlpxConnection.copy(uriOpt = Some(uri)), numRetries)

      case RLPxConnectionHandler.ConnectionFailed =>
        log.debug("Failed to establish RLPx connection")
        rlpxConnection.uriOpt match {
          case Some(uri) if numRetries < peerConfiguration.connectMaxRetries =>
            context unwatch rlpxConnection.ref
            scheduleConnectRetry(uri, numRetries)
          case Some(uri) =>
            log.debug("No more reconnect attempts left, removing peer")
            knownNodesManager ! KnownNodesManager.RemoveKnownNode(uri)
            context stop self
          case None =>
            log.debug("Connection was initiated by remote peer, not attempting to reconnect")
            context stop self
        }

      case GetStatus => sender() ! StatusResponse(Connecting)
    }

  def processingHandshaking(handshaker: Handshaker[R], rlpxConnection: RLPxConnection,
                            timeout: Cancellable, numRetries: Int): Receive =
      handleTerminated(rlpxConnection) orElse
      handleDisconnectMsg(rlpxConnection) orElse
      handlePingMsg(rlpxConnection) orElse stashMessages orElse {

      case RLPxConnectionHandler.MessageReceived(msg) =>
        // Processes the received message, cancels the timeout and processes a new message but only if the handshaker
        // handles the received message
        handshaker.applyMessage(msg).foreach{ newHandshaker =>
          timeout.cancel()
          processHandshakerNextMessage(newHandshaker, rlpxConnection, numRetries)
        }
        handshaker.respondToRequest(msg).foreach(msgToSend => rlpxConnection.sendMessage(msgToSend))

      case ResponseTimeout =>
        timeout.cancel()
        val newHandshaker = handshaker.processTimeout
        processHandshakerNextMessage(newHandshaker, rlpxConnection, numRetries)

      case GetStatus => sender() ! StatusResponse(Handshaking(numRetries))

    }

  /**
    * Asks for the next message to send to the handshaker, or, if there is None,
    * becomes MessageHandler if handshake was successful or disconnects from the peer otherwise
    *
    * @param handshaker
    * @param rlpxConnection
    * @param numRetries, number of connection retries done during RLPxConnection establishment
    */
  private def processHandshakerNextMessage(handshaker: Handshaker[R],
                                           rlpxConnection: RLPxConnection,
                                           numRetries: Int): Unit =
    handshaker.nextMessage match {
      case Right(NextMessage(msgToSend, timeoutTime)) =>
        rlpxConnection.sendMessage(msgToSend)
        val newTimeout = scheduler.scheduleOnce(timeoutTime, self, ResponseTimeout)
        context become processingHandshaking(handshaker, rlpxConnection, newTimeout, numRetries)

      case Left(HandshakeSuccess(handshakeResult)) =>
        rlpxConnection.uriOpt.foreach { uri => knownNodesManager ! KnownNodesManager.AddKnownNode(uri) }
        context become new HandshakedPeer(rlpxConnection, handshakeResult).receive
        unstashAll()

      case Left(HandshakeFailure(reason)) =>
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

  def disconnected: Receive = {
    case GetStatus => sender() ! StatusResponse(Disconnected)
  }

  def handleTerminated(rlpxConnection: RLPxConnection): Receive = {
    case Terminated(actor) if actor == rlpxConnection.ref =>
      log.debug(s"Underlying rlpx connection with peer $peerId closed")
      rlpxConnection.uriOpt match {
        case Some(uri) if rlpxConnection.isInitiator => scheduleConnectRetry(uri, numRetries = 0)
        case Some(uri) =>
          knownNodesManager ! KnownNodesManager.RemoveKnownNode(uri)
          context stop self
        case None =>
          context stop self
      }
  }

  def reconnect(uri: URI, numRetries: Int): Unit = {
    log.debug("Trying to reconnect")
    val address = new InetSocketAddress(uri.getHost, uri.getPort)
    val newConnection = createRlpxConnection(address, Some(uri), true)
    newConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
    context become waitingForConnectionResult(newConnection, numRetries)
  }

  def handlePingMsg(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(_: Ping) => rlpxConnection.sendMessage(Pong())
  }

  def handleDisconnectMsg(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(d: Disconnect) =>
      import Disconnect.Reasons._
      d.reason match {
        case IncompatibleP2pProtocolVersion | UselessPeer | NullNodeIdentityReceived | UnexpectedIdentity | IdentityTheSame | Other =>
          rlpxConnection.uriOpt.foreach(uri => knownNodesManager ! KnownNodesManager.RemoveKnownNode(uri))
        case _ => // nothing
      }
      log.debug(s"Received {}. Closing connection with peer ${peerAddress.getHostString}:${peerAddress.getPort}", d)
      context unwatch rlpxConnection.ref
      context stop self
  }

  def stashMessages: Receive = {
    case _: SendMessage | _: DisconnectPeer => stash()
  }

  class HandshakedPeer(rlpxConnection: RLPxConnection, handshakeResult: R) {

    peerEventBus ! Publish(PeerHandshakeSuccessful(peer, handshakeResult))

    /**
      * main behavior of actor that handles peer communication and subscriptions for messages
      */
    def receive: Receive =
      handlePingMsg(rlpxConnection) orElse
      handleDisconnectMsg(rlpxConnection) orElse
      handleTerminated(rlpxConnection) orElse {

        case RLPxConnectionHandler.MessageReceived(message) =>
          log.debug(s"Received message: {} from $peerId", message)
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
  def props[R <: HandshakeResult](peerAddress: InetSocketAddress,
                                  peerConfiguration: PeerConfiguration,
                                  peerEventBus: ActorRef,
                                  knownNodesManager: ActorRef,
                                  incomingConnection: Boolean,
                                  handshaker: Handshaker[R],
                                  authHandshaker: AuthHandshaker,
                                  messageDecoder: MessageDecoder): Props =
    Props(new PeerActor(
      peerAddress,
      rlpxConnectionFactory(authHandshaker, messageDecoder, peerConfiguration.rlpxConfiguration),
      peerConfiguration,
      peerEventBus,
      knownNodesManager,
      incomingConnection,
      initHandshaker = handshaker))

  def rlpxConnectionFactory(authHandshaker: AuthHandshaker, messageDecoder: MessageDecoder,
                            rlpxConfiguration: RLPxConfiguration): ActorContext => ActorRef = { ctx =>
    ctx.actorOf(
      RLPxConnectionHandler.props(NetworkMessageDecoder orElse messageDecoder, Versions.PV63, authHandshaker, rlpxConfiguration),
      "rlpx-connection")
  }

  case class RLPxConnection(ref: ActorRef, remoteAddress: InetSocketAddress, uriOpt: Option[URI], isInitiator: Boolean) {
    def sendMessage(message: MessageSerializable): Unit = {
      ref ! RLPxConnectionHandler.SendMessage(message)
    }
  }

  case class HandleConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectTo(uri: URI)

  case class SendMessage(message: MessageSerializable)

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
