package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import akka.actor._
import akka.agent.Agent
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.MessageHandler.MessageAction.TransmitMessage
import io.iohk.ethereum.network.MessageHandler.MessageHandlingResult
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.messages.{Versions, CommonMessages => msg}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.PeerActor.Status._
import io.iohk.ethereum.utils.NodeStatus
import io.iohk.ethereum.network.PeerMessageBusActor.{MessageFromPeer, Publish}
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.handshaker.Handshaker.HandshakeComplete.{HandshakeFailure, HandshakeSuccess}
import io.iohk.ethereum.network.handshaker.Handshaker.NextMessage
import io.iohk.ethereum.network.p2p.messages.PV62.GetBlockHeaders
import org.spongycastle.crypto.AsymmetricCipherKeyPair


/**
  * Peer actor is responsible for initiating and handling high-level connection with peer.
  * It creates child RLPxConnectionActor for handling underlying RLPx communication.
  * Once RLPx connection is established it proceeds with protocol handshake (i.e `Hello`
  * and `Status` exchange).
  * Once that's done it can send/receive messages with peer (HandshakedHandler.receive).
  */
//FIXME: MessageHandler type should be configurable, this is dependant on PR 185
class PeerActor(
    peerAddress: InetSocketAddress,
    rlpxConnectionFactory: ActorContext => ActorRef,
    val peerConfiguration: PeerConfiguration,
    peerMessageBus: ActorRef,
    externalSchedulerOpt: Option[Scheduler] = None,
    initHandshaker: Handshaker[EtcPeerInfo],
    messageHandlerBuilder: (EtcPeerInfo, Peer) => MessageHandler[EtcPeerInfo, EtcPeerInfo])
  extends Actor with ActorLogging with Stash {

  import PeerActor._
  import context.{dispatcher, system}

  def scheduler: Scheduler = externalSchedulerOpt getOrElse system.scheduler

  val P2pVersion = 4

  val peerId: PeerId = PeerId(self.path.name)
  val peer: Peer = Peer(peerAddress, self)

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

  def waitingForConnectionResult(rlpxConnection: RLPxConnection, numRetries: Int = 0): Receive =
    handleTerminated(rlpxConnection) orElse stashMessages orElse {
      case RLPxConnectionHandler.ConnectionEstablished =>
        processHandshakerNextMessage(initHandshaker, rlpxConnection, numRetries)

      case RLPxConnectionHandler.ConnectionFailed =>
        log.warning("Failed to establish RLPx connection")
        rlpxConnection.uriOpt match {
          case Some(uri) if numRetries < peerConfiguration.connectMaxRetries =>
            context unwatch rlpxConnection.ref
            scheduleConnectRetry(uri, numRetries)
          case Some(_) =>
            log.warning("No more reconnect attempts left, removing peer")
            context stop self
          case None =>
            log.warning("Connection was initiated by remote peer, not attempting to reconnect")
            context stop self
        }

      case GetStatus => sender() ! StatusResponse(Connecting)
    }

  def processingHandshaking(handshaker: Handshaker[EtcPeerInfo], rlpxConnection: RLPxConnection,
                            timeout: Cancellable, numRetries: Int): Receive =
      handleTerminated(rlpxConnection) orElse handleDisconnectMsg orElse
      handlePingMsg(rlpxConnection) orElse stashMessages orElse {

      case RLPxConnectionHandler.MessageReceived(msg) =>
        // Processes the received message, cancels the timeout and processes a new message but only if the handshaker
        // handles the received message
        handshaker.applyMessage(msg).foreach{ newHandshaker =>
          timeout.cancel()
          processHandshakerNextMessage(newHandshaker, rlpxConnection, numRetries)
        }
        handshaker.respondToRequest(msg).foreach{
          case msgToSend => rlpxConnection.sendMessage(msgToSend)
        }

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
  private def processHandshakerNextMessage(handshaker: Handshaker[EtcPeerInfo],
                                           rlpxConnection: RLPxConnection, numRetries: Int): Unit =
    handshaker.nextMessage match {
      case Right(NextMessage(msgToSend, timeoutTime)) =>
        rlpxConnection.sendMessage(msgToSend)
        val newTimeout = scheduler.scheduleOnce(timeoutTime, self, ResponseTimeout)
        context become processingHandshaking(handshaker, rlpxConnection, newTimeout, numRetries)

      case Left(HandshakeSuccess(EtcPeerInfo(initialStatus, totalDifficulty, forkAccepted, currentMaxBlockNumber))) =>
        startMessageHandler(rlpxConnection, initialStatus, totalDifficulty, forkAccepted, currentMaxBlockNumber)

      case Left(HandshakeFailure(reason)) =>
        disconnectFromPeer(rlpxConnection, reason)

    }

  private def scheduleConnectRetry(uri: URI, numRetries: Int): Unit = {
    log.info("Scheduling connection retry in {}", peerConfiguration.connectRetryDelay)
    scheduler.scheduleOnce(peerConfiguration.connectRetryDelay, self, RetryConnectionTimeout)
    context become {
      case RetryConnectionTimeout => reconnect(uri, numRetries + 1)
      case GetStatus => sender() ! StatusResponse(Connecting)
    }
  }

  private def startMessageHandler(rlpxConnection: RLPxConnection, remoteStatus: msg.Status,
                                  totalDifficulty: BigInt, forkAccepted: Boolean, currentMaxBlockNumber: BigInt): Unit = {
    val peerInfo = EtcPeerInfo(remoteStatus, totalDifficulty, forkAccepted, currentMaxBlockNumber)
    val messageHandler = messageHandlerBuilder(peerInfo, peer)
    context become new HandshakedPeer(rlpxConnection, messageHandler).receive
    unstashAll()
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
      log.warning(s"Underlying rlpx connection with peer $peerId closed")
      rlpxConnection.uriOpt match {
        case Some(uri) => scheduleConnectRetry(uri, numRetries = 0)
        case None => context stop self
      }
  }

  def reconnect(uri: URI, numRetries: Int): Unit = {
    log.info("Trying to reconnect")
    val address = new InetSocketAddress(uri.getHost, uri.getPort)
    val newConnection = createRlpxConnection(address, Some(uri))
    newConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
    context become waitingForConnectionResult(newConnection, numRetries)
  }

  def handlePingMsg(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(_: Ping) => rlpxConnection.sendMessage(Pong())
  }

  def handleDisconnectMsg: Receive = {
    case RLPxConnectionHandler.MessageReceived(d: Disconnect) =>
      log.info("Received {}. Closing connection", d)
      context stop self
  }

  def stashMessages: Receive = {
    case _: SendMessage | _: DisconnectPeer => stash()
  }

  class HandshakedPeer(rlpxConnection: RLPxConnection,
                       messageHandler: MessageHandler[EtcPeerInfo, EtcPeerInfo]) {

    /**
      * main behavior of actor that handles peer communication and subscriptions for messages
      */
    def receive: Receive =
      handlePingMsg(rlpxConnection) orElse
      handleDisconnectMsg orElse
      handleTerminated(rlpxConnection) orElse {

        case RLPxConnectionHandler.MessageReceived(message) =>
          log.debug("Received message: {}", message)
          val MessageHandlingResult(newHandler, messageAction) = messageHandler.receivingMessage(message)
          if(messageAction == TransmitMessage)
            peerMessageBus ! Publish(MessageFromPeer(message, peerId))
          context become new HandshakedPeer(rlpxConnection, newHandler).receive

        case DisconnectPeer(reason) =>
          disconnectFromPeer(rlpxConnection, reason)

        case SendMessage(message) =>
          val MessageHandlingResult(newHandler, messageAction) = messageHandler.sendingMessage(message)
          if(messageAction == TransmitMessage)
            rlpxConnection.sendMessage(message)
          context become new HandshakedPeer(rlpxConnection, newHandler).receive

        case GetStatus =>
          sender() ! StatusResponse(
            Handshaked(messageHandler.peerInfo.remoteStatus,
              messageHandler.peerInfo.forkAccepted, messageHandler.peerInfo.totalDifficulty))

    }
  }

}

object PeerActor {
  def props(peerAddress: InetSocketAddress,
            nodeStatusHolder: Agent[NodeStatus],
            peerConfiguration: PeerConfiguration,
            peerMessageBus: ActorRef,
            handshaker: Handshaker[EtcPeerInfo],
            messageHandlerBuilder: (EtcPeerInfo, Peer) => MessageHandler[EtcPeerInfo, EtcPeerInfo]): Props =
    Props(new PeerActor(
      peerAddress,
      rlpxConnectionFactory(nodeStatusHolder().key),
      peerConfiguration,
      peerMessageBus,
      initHandshaker = handshaker,
      messageHandlerBuilder = messageHandlerBuilder))

  def rlpxConnectionFactory(nodeKey: AsymmetricCipherKeyPair): ActorContext => ActorRef = { ctx =>
    // FIXME This message decoder should be configurable
    ctx.actorOf(RLPxConnectionHandler.props(nodeKey, EthereumMessageDecoder, Versions.PV63), "rlpx-connection")
  }

  case class RLPxConnection(ref: ActorRef, remoteAddress: InetSocketAddress, uriOpt: Option[URI]) {
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
    case class Handshaked(initialStatus: msg.Status, forkAccepted: Boolean, totalDifficulty: BigInt) extends Status
    case object Disconnected extends Status
  }
}
