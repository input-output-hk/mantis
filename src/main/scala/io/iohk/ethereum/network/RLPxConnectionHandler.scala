package io.iohk.ethereum.network

import java.net.{URI, InetSocketAddress}

import scala.concurrent.duration._

import akka.actor.{Cancellable, ActorLogging, ActorRef, Actor}
import akka.io.{Tcp, IO}
import akka.io.Tcp._
import akka.util.ByteString
import io.iohk.ethereum.crypto.ECIESCoder
import io.iohk.ethereum.network.p2p.{Message, Frame, FrameCodec}
import io.iohk.ethereum.rlp
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

  override def receive = waitingForCommand

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
      log.warning("Connection failed to peer {}", uri)
      context stop self
  }

  def waitingForAuthHandshakeInit(connection: ActorRef, handshaker: AuthHandshaker, timeout: Cancellable): Receive =
    handleTimeout orElse {
      case Received(data) =>
        timeout.cancel()
        val packetLength = AuthInitiateMessage.encodedLength + ECIESCoder.getOverhead
        val (handshakeData, remainingData) = data.splitAt(packetLength)
        val (responsePacket, result) = handshaker.handleInitialMessage(handshakeData)
        connection ! Write(responsePacket)
        processHandshakeResult(result, remainingData, connection)
    }

  def waitingForAuthHandshakeResponse(connection: ActorRef, handshaker: AuthHandshaker, timeout: Cancellable): Receive =
    handleWriteFailed orElse handleTimeout orElse {
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
        context.parent ! ConnectionEstablished
        val frameCodec = new FrameCodec(secrets)
        val framesSoFar = frameCodec.readFrames(remainingData)
        framesSoFar foreach processFrame
        context become connected(connection, frameCodec)

      case AuthHandshakeError =>
        context.parent ! ConnectionFailed(null)
        log.warning("Auth handshake with peer {} failed", peerId)
        context stop self
    }

  def processFrame(frame: Frame): Unit = {
    Try(Message.decode(frame.`type`, frame.payload)) match {
      case Success(message) =>
        context.parent ! MessageReceived(message)
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

  def connected(connection: ActorRef, frameCodec: FrameCodec): Receive = handleWriteFailed orElse {
    case sm: SendMessage[_] =>
      val frame = frameCodec.writeFrame(sm.message.code, ByteString(rlp.encode(sm.message)(sm.enc)))
      connection ! Write(frame)

    case Received(data) =>
      val frames = frameCodec.readFrames(data)
      frames foreach processFrame
  }
}

object RLPxConnectionHandler {
  case class ConnectTo(uri: URI)
  case class HandleConnection(connection: ActorRef)

  case object ConnectionEstablished
  case class ConnectionFailed(reason: ConnectionFailReason)
  sealed trait ConnectionFailReason

  case class MessageReceived(message: Message)
  case class SendMessage[M <: Message](message: M)(implicit val enc: RLPEncoder[M])

  private case object AuthHandshakeTimeout
}
