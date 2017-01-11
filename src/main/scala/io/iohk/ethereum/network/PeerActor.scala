package io.iohk.ethereum.network

import java.net.URI

import scala.concurrent.duration._

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.network.RLPxConnectionHandler.MessageReceived
import io.iohk.ethereum.network.p2p._
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters

class PeerActor(nodeKey: AsymmetricCipherKeyPair) extends Actor with ActorLogging {

  import PeerActor._
  import context.{dispatcher, system}

  val nodeId = nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId

  val peerId = self.path.name

  // supervisor strategy
  // watch rlpx conn

  override def receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = {
    case HandleConnection(connection) =>
      val rlpxConnectionActor = context.actorOf(Props(new RLPxConnectionHandler(nodeKey)), "rlpx-connection")
      rlpxConnectionActor ! RLPxConnectionHandler.HandleConnection(connection)
      context become waitingForConnectionResult(rlpxConnectionActor)

    case ConnectTo(uri) =>
      val rlpxConnectionActor = context.actorOf(Props(new RLPxConnectionHandler(nodeKey)), "rlpx-connection")
      rlpxConnectionActor ! RLPxConnectionHandler.ConnectTo(uri)
      context become waitingForConnectionResult(rlpxConnectionActor)
  }

  def waitingForConnectionResult(rlpxConnection: ActorRef): Receive = {
    case RLPxConnectionHandler.ConnectionEstablished =>
      val hello = Hello(
        p2pVersion = 4,
        clientId = "etc-client",
        capabilities = Seq(Capability("eth", 62.toByte)),
        listenPort = 3333,
        nodeId = ByteString(nodeId))
      rlpxConnection ! RLPxConnectionHandler.SendMessage(hello)
      val timeout = system.scheduler.scheduleOnce(3.seconds, self, ProtocolHandshakeTimeout)
      context become waitingForHello(rlpxConnection, timeout)

    case RLPxConnectionHandler.ConnectionFailed(reason) =>
      context stop self
  }

  def waitingForHello(rlpxConnection: ActorRef, timeout: Cancellable): Receive = {
    case MessageReceived(hello: Hello) =>
      log.info("Protocol handshake finished with peer {}", peerId)
      timeout.cancel()
      // check protocols
      context become handshaked(rlpxConnection)
  }

  def handshaked(rlpxConnection: ActorRef): Receive = {
    case RLPxConnectionHandler.MessageReceived(message) =>
      // notify subscribers ?

    case Ping() =>
      rlpxConnection ! Pong()
  }
}

object PeerActor {
  def props(nodeKey: AsymmetricCipherKeyPair) = Props(new PeerActor(nodeKey))

  case class HandleConnection(connection: ActorRef)
  case class ConnectTo(uri: URI)

  private case object ProtocolHandshakeTimeout
}
