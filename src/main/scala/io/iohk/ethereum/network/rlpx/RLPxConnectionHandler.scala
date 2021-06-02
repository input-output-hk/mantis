package io.iohk.ethereum.network.rlpx

import java.net.{InetSocketAddress, URI}
import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.p2p.{Message, MessageDecoder, MessageSerializable, NetworkMessageDecoder}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.utils.ByteUtils
import org.bouncycastle.util.encoders.Hex

import scala.collection.immutable.Queue
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * This actors takes care of initiating a secure connection (auth handshake) between peers.
  * Once such connection is established it allows to send/receive frames (messages) over it.
  *
  * The actor can be in one of four states:
  * 1. when created it waits for initial command (either handle incoming connection or connect using uri)
  * 2. when new connection is requested the actor waits for the result (waitingForConnectionResult)
  * 3. once underlying connection is established it either waits for handshake init message or for response message
  * (depending on who initiated the connection)
  * 4. once handshake is done (and secure connection established) actor can send/receive messages (`handshaked` state)
  */
class RLPxConnectionHandler(
    messageDecoder: MessageDecoder,
    capabilities: List[Capability],
    authHandshaker: AuthHandshaker,
    messageCodecFactory: (FrameCodec, MessageDecoder, Capability, Long) => MessageCodec,
    rlpxConfiguration: RLPxConfiguration
) extends Actor
    with ActorLogging {

  import AuthHandshaker.{InitiatePacketLength, ResponsePacketLength}
  import RLPxConnectionHandler._
  import context.{dispatcher, system}

  val peerId: String = context.parent.path.name

  override def receive: Receive = waitingForCommand

  def tcpActor: ActorRef = IO(Tcp)

  def waitingForCommand: Receive = {
    case ConnectTo(uri) =>
      tcpActor ! Connect(new InetSocketAddress(uri.getHost, uri.getPort))
      context become waitingForConnectionResult(uri)

    case HandleConnection(connection) =>
      connection ! Register(self)
      val timeout = system.scheduler.scheduleOnce(rlpxConfiguration.waitForHandshakeTimeout, self, AuthHandshakeTimeout)
      context become new ConnectedHandler(connection).waitingForAuthHandshakeInit(authHandshaker, timeout)
  }

  def waitingForConnectionResult(uri: URI): Receive = {
    case Connected(_, _) =>
      val connection = sender()
      connection ! Register(self)
      val (initPacket, handshaker) = authHandshaker.initiate(uri)
      connection ! Write(initPacket)
      val timeout = system.scheduler.scheduleOnce(rlpxConfiguration.waitForHandshakeTimeout, self, AuthHandshakeTimeout)
      context become new ConnectedHandler(connection).waitingForAuthHandshakeResponse(handshaker, timeout)

    case CommandFailed(_: Connect) =>
      log.debug("[Stopping Connection] Connection to {} failed", uri)
      context.parent ! ConnectionFailed
      context stop self
  }

  class ConnectedHandler(connection: ActorRef) {

    def waitingForAuthHandshakeInit(handshaker: AuthHandshaker, timeout: Cancellable): Receive =
      handleTimeout orElse handleConnectionClosed orElse { case Received(data) =>
        timeout.cancel()
        // FIXME EIP8 is 6 years old, time to drop it
        val maybePreEIP8Result = Try {
          val (responsePacket, result) = handshaker.handleInitialMessage(data.take(InitiatePacketLength))
          val remainingData = data.drop(InitiatePacketLength)
          (responsePacket, result, remainingData)
        }
        lazy val maybePostEIP8Result = Try {
          val (packetData, remainingData) = decodeV4Packet(data)
          val (responsePacket, result) = handshaker.handleInitialMessageV4(packetData)
          (responsePacket, result, remainingData)
        }

        maybePreEIP8Result orElse maybePostEIP8Result match {
          case Success((responsePacket, result, remainingData)) =>
            connection ! Write(responsePacket)
            processHandshakeResult(result, remainingData)

          case Failure(ex) =>
            log.debug(
              s"[Stopping Connection] Init AuthHandshaker message handling failed for peer $peerId due to ${ex.getMessage}"
            )
            context.parent ! ConnectionFailed
            context stop self
        }
      }

    def waitingForAuthHandshakeResponse(handshaker: AuthHandshaker, timeout: Cancellable): Receive =
      handleWriteFailed orElse handleTimeout orElse handleConnectionClosed orElse { case Received(data) =>
        timeout.cancel()
        val maybePreEIP8Result = Try {
          val result = handshaker.handleResponseMessage(data.take(ResponsePacketLength))
          val remainingData = data.drop(ResponsePacketLength)
          (result, remainingData)
        }
        val maybePostEIP8Result = Try {
          val (packetData, remainingData) = decodeV4Packet(data)
          val result = handshaker.handleResponseMessageV4(packetData)
          (result, remainingData)
        }
        maybePreEIP8Result orElse maybePostEIP8Result match {
          case Success((result, remainingData)) =>
            processHandshakeResult(result, remainingData)

          case Failure(ex) =>
            log.debug(
              s"[Stopping Connection] Response AuthHandshaker message handling failed for peer $peerId due to ${ex.getMessage}"
            )
            context.parent ! ConnectionFailed
            context stop self
        }
      }

    /**
      * Decode V4 packet
      *
      * @param data , includes both the V4 packet with bytes from next messages
      * @return data of the packet and the remaining data
      */
    private def decodeV4Packet(data: ByteString): (ByteString, ByteString) = {
      val encryptedPayloadSize = ByteUtils.bigEndianToShort(data.take(2).toArray)
      val (packetData, remainingData) = data.splitAt(encryptedPayloadSize + 2)
      packetData -> remainingData
    }

    def handleTimeout: Receive = { case AuthHandshakeTimeout =>
      log.debug(s"[Stopping Connection] Auth handshake timeout for peer $peerId")
      context.parent ! ConnectionFailed
      context stop self
    }

    def processHandshakeResult(result: AuthHandshakeResult, remainingData: ByteString): Unit =
      result match {
        case AuthHandshakeSuccess(secrets, remotePubKey) =>
          log.info(s"Auth handshake succeeded for peer $peerId")

          //expect Hello
          val frameCodec = new FrameCodec(secrets)
          val frames = frameCodec.readFrames(remainingData)
          frames.headOption.flatMap(extractHello) match {
            case Some(h) =>
              val protocolVersion = Capability.negotiate(h.capabilities.toList, capabilities)
              val p2pVersion = h.p2pVersion
              protocolVersion match {
                case Some(value) =>
                  context.parent ! ConnectionEstablished(remotePubKey, value)
                  context.parent ! MessageReceived(h)
                  val messageCodec = messageCodecFactory(frameCodec, messageDecoder, value, p2pVersion)
                  val restFrames = frames.drop(1)
                  if (restFrames.nonEmpty) {
                    val messagesSoFar = messageCodec.readFrames(restFrames) // omit hello
                    messagesSoFar foreach processMessage
                  }
                  context become handshaked(messageCodec)
                case None =>
                  log.debug(s"[Stopping Connection] Unable to connect to $peerId, no common capability found ${h.capabilities} and $capabilities")
                  context.parent ! ConnectionFailed
                  context stop self
              }

            case None =>
              log.debug(s"[Stopping Connection] Unable to connect to $peerId, 'Hello' not found")
              context.parent ! ConnectionFailed
              context stop self
          }

        case AuthHandshakeError =>
          log.debug(s"[Stopping Connection] Auth handshake failed for peer $peerId")
          context.parent ! ConnectionFailed
          context stop self
      }

    private def extractHello(frame: Frame): Option[Hello] = {
      val frameData = frame.payload.toArray
      if(frame.`type` == Hello.code) {
        val m = NetworkMessageDecoder.fromBytes(frame.`type`, frameData, Capability.Capabilities.Eth63Capability)
        Some(m.asInstanceOf[Hello])
      } else {
        log.error("Unable to find 'Hello'")
        None
      }
    }

    def processMessage(messageTry: Try[Message]): Unit = messageTry match {
      case Success(message) =>
        context.parent ! MessageReceived(message)

      case Failure(ex) =>
        log.info(s"Cannot decode message from $peerId, because of ${ex.getMessage}")
        // break connection in case of failed decoding, to avoid attack which would send us garbage
        context stop self
    }

    /**
      * Handles sending and receiving messages from the Akka TCP connection, while also handling acknowledgement of
      * messages sent. Messages are only sent when all Ack from previous messages were received.
      *
      * @param messageCodec          , for encoding the messages sent
      * @param messagesNotSent       , messages not yet sent
      * @param cancellableAckTimeout , timeout for the message sent for which we are awaiting an acknowledgement (if there is one)
      * @param seqNumber             , sequence number for the next message to be sent
      */
    def handshaked(
        messageCodec: MessageCodec,
        messagesNotSent: Queue[MessageSerializable] = Queue.empty,
        cancellableAckTimeout: Option[CancellableAckTimeout] = None,
        seqNumber: Int = 0
    ): Receive = {
      handleWriteFailed orElse handleConnectionClosed orElse {
        case sm: SendMessage =>
          if (cancellableAckTimeout.isEmpty)
            sendMessage(messageCodec, sm.serializable, seqNumber, messagesNotSent)
          else
            context become handshaked(
              messageCodec,
              messagesNotSent :+ sm.serializable,
              cancellableAckTimeout,
              seqNumber
            )

        case Received(data) =>
          val messages = messageCodec.readMessages(data)
          messages foreach processMessage

        case Ack if cancellableAckTimeout.nonEmpty =>
          //Cancel pending message timeout
          cancellableAckTimeout.foreach(_.cancellable.cancel())

          //Send next message if there is one
          if (messagesNotSent.nonEmpty)
            sendMessage(messageCodec, messagesNotSent.head, seqNumber, messagesNotSent.tail)
          else
            context become handshaked(messageCodec, Queue.empty, None, seqNumber)

        case AckTimeout(ackSeqNumber) if cancellableAckTimeout.exists(_.seqNumber == ackSeqNumber) =>
          cancellableAckTimeout.foreach(_.cancellable.cancel())
          log.debug(s"[Stopping Connection] Write to $peerId failed")
          context stop self
      }
    }

    /**
      * Sends an encoded message through the TCP connection, an Ack will be received when the message was
      * successfully queued for delivery. A cancellable timeout is created for the Ack message.
      *
      * @param messageCodec        , for encoding the messages sent
      * @param messageToSend       , message to be sent
      * @param seqNumber           , sequence number for the message to be sent
      * @param remainingMsgsToSend , messages not yet sent
      */
    private def sendMessage(
        messageCodec: MessageCodec,
        messageToSend: MessageSerializable,
        seqNumber: Int,
        remainingMsgsToSend: Queue[MessageSerializable]
    ): Unit = {
      val out = messageCodec.encodeMessage(messageToSend)
      connection ! Write(out, Ack)
      log.debug(s"Sent message: ${messageToSend.underlyingMsg.toShortString} to $peerId")

      val timeout = system.scheduler.scheduleOnce(rlpxConfiguration.waitForTcpAckTimeout, self, AckTimeout(seqNumber))
      context become handshaked(
        messageCodec = messageCodec,
        messagesNotSent = remainingMsgsToSend,
        cancellableAckTimeout = Some(CancellableAckTimeout(seqNumber, timeout)),
        seqNumber = increaseSeqNumber(seqNumber)
      )
    }

    /**
      * Given a sequence number for the AckTimeouts, the next seq number is returned
      *
      * @param seqNumber , the current sequence number
      * @return the sequence number for the next message sent
      */
    private def increaseSeqNumber(seqNumber: Int): Int = seqNumber match {
      case Int.MaxValue => 0
      case _ => seqNumber + 1
    }

    def handleWriteFailed: Receive = { case CommandFailed(cmd: Write) =>
      log.debug(
        s"[Stopping Connection] Write to peer $peerId failed, trying to send ${Hex.toHexString(cmd.data.toArray[Byte])}"
      )
      context stop self
    }

    def handleConnectionClosed: Receive = { case msg: ConnectionClosed =>
      if (msg.isPeerClosed) {
        log.debug(s"[Stopping Connection] Connection with $peerId closed by peer")
      }
      if (msg.isErrorClosed) {
        log.debug(s"[Stopping Connection] Connection with $peerId closed because of error ${msg.getErrorCause}")
      }

      context stop self
    }
  }

}

object RLPxConnectionHandler {
  def props(
      messageDecoder: MessageDecoder,
      capabilities: List[Capability],
      authHandshaker: AuthHandshaker,
      rlpxConfiguration: RLPxConfiguration
  ): Props =
    Props(
      new RLPxConnectionHandler(messageDecoder, capabilities, authHandshaker, messageCodecFactory, rlpxConfiguration)
    )

  def messageCodecFactory(
      frameCodec: FrameCodec,
      messageDecoder: MessageDecoder,
      protocolVersion: Capability,
      p2pVersion: Long
  ): MessageCodec =
    new MessageCodec(frameCodec, messageDecoder, protocolVersion, p2pVersion)

  case class ConnectTo(uri: URI)

  case class HandleConnection(connection: ActorRef)

  case class ConnectionEstablished(nodeId: ByteString, negotiatedCapability: Capability)

  case object ConnectionFailed

  case class MessageReceived(message: Message)

  case class SendMessage(serializable: MessageSerializable)

  private case object AuthHandshakeTimeout

  case object Ack extends Tcp.Event

  case class AckTimeout(seqNumber: Int)

  case class CancellableAckTimeout(seqNumber: Int, cancellable: Cancellable)

  trait RLPxConfiguration {
    val waitForHandshakeTimeout: FiniteDuration
    val waitForTcpAckTimeout: FiniteDuration
  }

}
