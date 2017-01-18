package io.iohk.ethereum.network

import java.net.{URI, InetSocketAddress}

import scala.concurrent.duration._

import akka.actor._
import akka.io.{Tcp, IO}
import akka.io.Tcp._
import akka.util.ByteString
import io.iohk.ethereum.crypto.ECIESCoder
import io.iohk.ethereum.network.p2p.{MessageCodec, Message, FrameCodec}
import io.iohk.ethereum.rlp.RLPEncoder
import org.spongycastle.crypto.AsymmetricCipherKeyPair

import scala.util.{Failure, Success, Try}

/**
  * This actors takes care of initiating a secure connection (auth handshake) between peers.
  * Once such connection is established it allows to send/receive frames (messages) over it.
  */
class RLPxConnectionHandler(nodeKey: AsymmetricCipherKeyPair)
  extends Actor with ActorLogging {

  import RLPxConnectionHandler._
  import context.{dispatcher, system}

  val peerId = context.parent.path.name

  override def receive: Receive = waitingForCommand

  def waitingForCommand: Receive = {
    case ConnectTo(uri) =>
      IO(Tcp) ! Connect(new InetSocketAddress(uri.getHost, uri.getPort))
      context become waitingForConnectionResult(uri)

    case HandleConnection(connection) =>
      connection ! Register(self)
      val handshaker = AuthHandshaker(nodeKey)
      val timeout = system.scheduler.scheduleOnce(3.seconds, self, AuthHandshakeTimeout)
      context become waitingForAuthHandshakeInit(connection, handshaker, timeout)
  }

  def waitingForConnectionResult(uri: URI): Receive = {
    case Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      val (initPacket, handshaker) = AuthHandshaker(nodeKey).initiate(uri)
      connection ! Write(initPacket)
      val timeout = system.scheduler.scheduleOnce(3.seconds, self, AuthHandshakeTimeout)
      context become waitingForAuthHandshakeResponse(connection, handshaker, timeout)

    case CommandFailed(_: Connect) =>
      log.warning("Connection failed to {}", uri)
      context stop self
  }

  def waitingForAuthHandshakeInit(connection: ActorRef, handshaker: AuthHandshaker, timeout: Cancellable): Receive =
    handleTimeout orElse handleConnectionClosed orElse {
      case Received(data) =>
        timeout.cancel()
        val packetLength = AuthInitiateMessage.encodedLength + ECIESCoder.getOverhead
        val (handshakeData, remainingData) = data.splitAt(packetLength)
        val (responsePacket, result) = handshaker.handleInitialMessage(handshakeData)
        connection ! Write(responsePacket)
        processHandshakeResult(result, remainingData, connection)
    }

  def waitingForAuthHandshakeResponse(connection: ActorRef, handshaker: AuthHandshaker, timeout: Cancellable): Receive =
    handleWriteFailed orElse handleTimeout orElse handleConnectionClosed orElse {
      case Received(data) =>
        timeout.cancel()
        val packetLength = AuthResponseMessage.encodedLength + ECIESCoder.getOverhead
        val (handshakeData, remainingData) = data.splitAt(packetLength)
        val result = handshaker.handleResponseMessage(handshakeData)
        processHandshakeResult(result, remainingData, connection)
    }

  def processHandshakeResult(result: AuthHandshakeResult, remainingData: ByteString, connection: ActorRef): Unit =
    result match {
      case AuthHandshakeSuccess(secrets) =>
        log.warning("Auth handshake with peer {} succeeded", peerId)
        context.parent ! ConnectionEstablished
        val messageCodec = new MessageCodec(new FrameCodec(secrets))
        val messagesSoFar = messageCodec.readMessages(remainingData)
        messagesSoFar foreach processMessage
        context become connected(connection, messageCodec)

      case AuthHandshakeError =>
        context.parent ! ConnectionFailed
        log.warning("Auth handshake with peer {} failed", peerId)
        context stop self
    }

  def processMessage(messageTry: Try[Message]): Unit = {
    messageTry match {
      case Success(message) => context.parent ! MessageReceived(message)
      case Failure(ex) => log.error(ex, "Cannot decode message from peer {}", peerId)
    }
  }

  def handleTimeout: Receive = {
    case AuthHandshakeTimeout =>
      log.info("Timeout during auth handshake with peer {}", peerId)
      context stop self
  }

  def handleWriteFailed: Receive = {
    case CommandFailed(_: Write) =>
      log.warning("Write failed to peer {}", peerId)
      context stop self
  }

  def handleConnectionClosed: Receive = {
    case _: ConnectionClosed =>
      context stop self
  }

  def connected(connection: ActorRef, messageCodec: MessageCodec): Receive =
    handleWriteFailed orElse handleConnectionClosed orElse {
      case sm: SendMessage[_] =>
        val out = messageCodec.encodeMessage(sm.message)(sm.enc)
        connection ! Write(out)

      case Received(data) =>
        val messages = messageCodec.readMessages(data)
        messages foreach processMessage
    }
}

object RLPxConnectionHandler {
  def props(nodeKey: AsymmetricCipherKeyPair): Props =
    Props(new RLPxConnectionHandler(nodeKey))

  case class ConnectTo(uri: URI)
  case class HandleConnection(connection: ActorRef)

  case object ConnectionEstablished
  case object ConnectionFailed

  case class MessageReceived(message: Message)
  case class SendMessage[M <: Message](message: M)(implicit val enc: RLPEncoder[M])

  private case object AuthHandshakeTimeout
}
