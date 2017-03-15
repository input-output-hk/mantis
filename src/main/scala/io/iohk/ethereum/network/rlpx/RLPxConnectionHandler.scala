package io.iohk.ethereum.network.rlpx

import java.net.{InetSocketAddress, URI}

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.ByteUtils
import org.spongycastle.crypto.AsymmetricCipherKeyPair

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * This actors takes care of initiating a secure connection (auth handshake) between peers.
  * Once such connection is established it allows to send/receive frames (messages) over it.
  *
  * The actor can be in one of four states:
  * 1. when created it waits for initial command (either handle incoming connection or connect usin g uri)
  * 2. when new connection is requested the actor waits for the result (waitingForConnectionResult)
  * 3. once underlying connection is established it either waits for handshake init message or for response message
  *    (depending on who initiated the connection)
  * 4. once handshake is done (and secure connection established) actor can send/receive messages (`handshaked` state)
  */
class RLPxConnectionHandler(nodeKey: AsymmetricCipherKeyPair)
  extends Actor with ActorLogging {

  import AuthHandshaker.{InitiatePacketLength, ResponsePacketLength}
  import RLPxConnectionHandler._
  import context.{dispatcher, system}

  val ProtocolVersion = Message.PV63

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
      context become new ConnectedHandler(connection).waitingForAuthHandshakeInit(handshaker, timeout)
  }

  def waitingForConnectionResult(uri: URI): Receive = {
    case Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      val (initPacket, handshaker) = AuthHandshaker(nodeKey).initiate(uri)
      connection ! Write(initPacket)
      val timeout = system.scheduler.scheduleOnce(3.seconds, self, AuthHandshakeTimeout)
      context become new ConnectedHandler(connection).waitingForAuthHandshakeResponse(handshaker, timeout)

    case CommandFailed(_: Connect) =>
      log.warning("Connection to {} failed", uri)
      context.parent ! ConnectionFailed
      context stop self
  }

  class ConnectedHandler(connection: ActorRef) {

    def waitingForAuthHandshakeInit(handshaker: AuthHandshaker, timeout: Cancellable): Receive =
      handleTimeout orElse handleConnectionClosed orElse {
        case Received(data) =>
          timeout.cancel()
          Try(handshaker.handleInitialMessage(data.take(InitiatePacketLength))) match {
            case Success((responsePacket, result)) =>
              // process pre-eip8 message
              val remainingData = data.drop(InitiatePacketLength)
              connection ! Write(responsePacket)
              processHandshakeResult(result, remainingData)

            case Failure(_) =>
              // process as eip8 message
              val encryptedPayloadSize = ByteUtils.bigEndianToShort(data.take(2).toArray)
              val (packetData, remainingData) = data.splitAt(encryptedPayloadSize + 2)
              val (responsePacket, result) = handshaker.handleInitialMessageV4(packetData)
              connection ! Write(responsePacket)
              processHandshakeResult(result, remainingData)
          }
      }

    def waitingForAuthHandshakeResponse(handshaker: AuthHandshaker, timeout: Cancellable): Receive =
      handleWriteFailed orElse handleTimeout orElse handleConnectionClosed orElse {
        case Received(data) =>
          timeout.cancel()
          Try(handshaker.handleResponseMessage(data.take(ResponsePacketLength))) match {
            case Success(result) =>
              // process pre-eip8 message
              val remainingData = data.drop(ResponsePacketLength)
              processHandshakeResult(result, remainingData)

            case Failure(_) =>
              // process as eip8 message
              val size = ByteUtils.bigEndianToShort(data.take(2).toArray)
              val (packetData, remainingData) = data.splitAt(size + 2)
              val result = handshaker.handleResponseMessageV4(packetData)
              processHandshakeResult(result, remainingData)
          }
      }

    def handleTimeout: Receive = {
      case AuthHandshakeTimeout =>
        log.debug("Auth handshake timeout")
        context.parent ! ConnectionFailed
        context stop self
    }

    def processHandshakeResult(result: AuthHandshakeResult, remainingData: ByteString): Unit =
      result match {
        case AuthHandshakeSuccess(secrets) =>
          log.debug("Auth handshake succeeded")
          context.parent ! ConnectionEstablished
          val messageCodec = new MessageCodec(new FrameCodec(secrets), ProtocolVersion)
          val messagesSoFar = messageCodec.readMessages(remainingData)
          messagesSoFar foreach processMessage
          context become handshaked(messageCodec)

        case AuthHandshakeError =>
          log.debug("Auth handshake failed")
          context.parent ! ConnectionFailed
          context stop self
      }

    def processMessage(messageTry: Try[Message]): Unit = messageTry match {
      case Success(message) =>
        context.parent ! MessageReceived(message)

      case Failure(ex) =>
        log.error(ex, "Cannot decode message")
    }

    def handshaked(messageCodec: MessageCodec): Receive =
      handleWriteFailed orElse handleConnectionClosed orElse {
        case sm: SendMessage[_] =>
          val out = messageCodec.encodeMessage(sm.message)(sm.enc)
          connection ! Write(out)
          log.debug("Sent message: {}", sm.message)

        case Received(data) =>
          val messages = messageCodec.readMessages(data)
          messages foreach processMessage
      }

    def handleWriteFailed: Receive = {
      case CommandFailed(_: Write) =>
        log.warning("Write failed")
        context stop self
    }

    def handleConnectionClosed: Receive = {
      case _: ConnectionClosed =>
        log.debug("Connection closed")
        context stop self
    }
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
