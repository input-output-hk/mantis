package io.iohk.ethereum.network

import java.net.URI

import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.rlp.RLPEncoder

import scala.concurrent.duration._

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.network.RLPxConnectionHandler.MessageReceived
import io.iohk.ethereum.network.p2p._

class PeerActor(nodeInfo: NodeInfo) extends Actor with ActorLogging {

  import PeerActor._
  import context.{dispatcher, system}

  val P2pVersion = 4
  val EthProtocolVersion = 63.toByte

  val peerId = self.path.name

  override def receive: Receive = waitingForInitialCommand

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

  def createRlpxConnection(): ActorRef = {
    val rlpxConnection = context.actorOf(RLPxConnectionHandler.props(nodeInfo), "rlpx-connection")
    context watch rlpxConnection
    rlpxConnection
  }

  def handleTerminated: Receive = {
    case _: Terminated => context stop self
  }

  def waitingForConnectionResult(rlpxConnection: ActorRef): Receive = handleTerminated orElse {
    case RLPxConnectionHandler.ConnectionEstablished =>
      val hello = Hello(
        p2pVersion = P2pVersion,
        clientId = "etc-client",
        capabilities = Seq(Capability("eth", EthProtocolVersion)),
        listenPort = nodeInfo.listenAddress.getPort,
        nodeId = ByteString(nodeInfo.nodeId))
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

      if (hello.capabilities.contains(Capability("eth", EthProtocolVersion))) {
        context become new HandshakedHandler(rlpxConnection).receive
      } else {
        rlpxConnection ! SendMessage(Disconnect(Disconnect.Reasons.IncompatibleP2pProtocolVersion))
        context.system.scheduler.scheduleOnce(5.seconds, self, PoisonPill)
        log.info("Connected peer does not support eth {} protocol. Disconnecting.", EthProtocolVersion)
      }
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
      case Ping() =>
        sendMessage(Pong())

      case d: Disconnect =>
        log.info("Received {} from peer {}. Closing connection", d, peerId)
        context stop self

      case msg =>
        log.info("Received message {} from {}", msg, peerId)
    }

  }

}

object PeerActor {
  def props(nodeInfo: NodeInfo): Props =
    Props(new PeerActor(nodeInfo))

  case class HandleConnection(connection: ActorRef)
  case class ConnectTo(uri: URI)

  case class SendMessage[M <: Message](message: M)(implicit val enc: RLPEncoder[M])

  private case object ProtocolHandshakeTimeout
}
