package io.iohk.ethereum.network

import java.net.URI

import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Ping, Pong, Capability, Hello}
import io.iohk.ethereum.rlp.RLPEncoder

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

  override def receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = {
    case HandleConnection(connection) =>
      val rlpxConnection = createRlpxConnection()
      rlpxConnection ! RLPxConnectionHandler.HandleConnection(connection)
      context become waitingForConnectionResult(rlpxConnection)

    case ConnectTo(uri) =>
      val rlpxConnection = createRlpxConnection()
      rlpxConnection ! RLPxConnectionHandler.ConnectTo(uri)
      context become waitingForConnectionResult(rlpxConnection)
  }

  def createRlpxConnection() = {
    val rlpxConnection = context.actorOf(Props(new RLPxConnectionHandler(nodeKey)), "rlpx-connection")
    context watch rlpxConnection
    rlpxConnection
  }

  def handleTerminated: Receive = {
    case _: Terminated => context stop self
  }

  def waitingForConnectionResult(rlpxConnection: ActorRef): Receive = handleTerminated orElse {
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

    case RLPxConnectionHandler.ConnectionFailed =>
      context stop self
  }

  def waitingForHello(rlpxConnection: ActorRef, timeout: Cancellable): Receive = handleTerminated orElse {
    case MessageReceived(hello: Hello) =>
      log.info("Protocol handshake finished with peer {} ({})", peerId, hello)
      timeout.cancel()
      // TODO: check which protocol to use
      context become new HandshakedHandler(rlpxConnection).receive
  }

  class HandshakedHandler(rlpxConnection: ActorRef) {

    def receive: Receive = handleTerminated orElse {
      case RLPxConnectionHandler.MessageReceived(message) => handleMessage(message)
      case s: SendMessage[_] => sendMessage(s.message)(s.enc)
    }

    def sendMessage[M <: Message : RLPEncoder](message: M): Unit = {
      rlpxConnection ! RLPxConnectionHandler.SendMessage(message)
    }

    def handleMessage(message: Message): Unit = message match {
      case Ping() => sendMessage(Pong())
      case _ =>
    }

  }

}

object PeerActor {
  def props(nodeKey: AsymmetricCipherKeyPair) = Props(new PeerActor(nodeKey))

  case class HandleConnection(connection: ActorRef)
  case class ConnectTo(uri: URI)

  case class SendMessage[M <: Message](message: M)(implicit val enc: RLPEncoder[M])

  private case object ProtocolHandshakeTimeout
}
