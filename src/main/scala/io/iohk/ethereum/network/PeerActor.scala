package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import io.iohk.ethereum.network.p2p.{Capability, FrameCodec, Hello, Message}
import io.iohk.ethereum.rlp
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

// TODO: handle write (and other?) errors
class PeerActor(id: String, nodeKey: AsymmetricCipherKeyPair) extends Actor with ActorLogging {

  import PeerActor._
  import context.system

  override def receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = {
    case HandleConnection(connection) =>
      connection ! Register(self)
      val handshaker = AuthHandshaker(nodeKey)
      context become new AuthHandshakeHandler(connection, handshaker).waitingForInit

    case ConnectTo(uri) =>
      IO(Tcp) ! Connect(new InetSocketAddress(uri.getHost, uri.getPort))
      context become waitingForConnectionResult(uri)
  }

  def waitingForConnectionResult(uri: URI): Receive = {
    case Connected(remote, local) =>
      val connection = sender()
      connection ! Register(self)
      val (initPacket, handshaker) = AuthHandshaker(nodeKey).initiate(uri)
      connection ! Write(initPacket)
      context become new AuthHandshakeHandler(connection, handshaker).waitingForResponse

    case CommandFailed(_: Connect) =>
      log.warning("Connection failed to peer {}", uri)
      context stop self
  }

  class AuthHandshakeHandler(connection: ActorRef, handshaker: AuthHandshaker) {

    def waitingForInit: Receive = {
      case Received(data) =>
        val (responsePacket, result) = handshaker.handleInitialMessage(data)
        connection ! Write(responsePacket)
        handleAuthHandshakeResult(result)
    }

    def waitingForResponse: Receive = {
      case Received(data) =>
        val result = handshaker.handleResponseMessage(data)
        handleAuthHandshakeResult(result)
    }

    def handleAuthHandshakeResult(result: AuthHandshakeResult) = result match {
      case AuthHandshakeSuccess(secrets) =>
        val frameCodec = new FrameCodec(secrets)
        val hello = Hello(
          p2pVersion = 4,
          clientId = "etc-client",
          capabilities = Seq(Capability("eth", 62.toByte)),
          listenPort = 3333,
          nodeId = ByteString(nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId))
        val encoded = rlp.encode(hello)
        val frame = frameCodec.writeFrame(hello.code, ByteString(encoded))
        connection ! Write(frame)
        context become new ProtocolHandshakeHandler(connection, null).waitingForResponse

      case AuthHandshakeError =>
        log.warning("Failed to handshake with peer")
        context stop self
    }

  }

  class ProtocolHandshakeHandler(connection: ActorRef, frameCodec: FrameCodec) {

    // TODO: add timeout

    def waitingForResponse: Receive = {
      case Received(data) =>
        val frames = frameCodec.readFrames(data)
        // TODO: handle multi-frame messages?
        val messages = frames.map(f => Message.decode(f.`type`, f.payload))
        val helloOpt = messages.collect { case h: Hello => h }.headOption
        helloOpt foreach { hello =>
          // compare versions etc etc
          // context become ... ?
        }
    }

  }

}

object PeerActor {
  def props(id: String, nodeKey: AsymmetricCipherKeyPair) = Props(new PeerActor(id, nodeKey))

  case class HandleConnection(connection: ActorRef)
  case class ConnectTo(uri: URI)
}
