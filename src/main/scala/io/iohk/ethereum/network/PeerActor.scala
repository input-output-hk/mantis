package io.iohk.ethereum.network

import java.math.BigInteger
import java.net.URI

import io.iohk.ethereum.network.p2p.messages.CommonMessages
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.rlp.RLPEncoder
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration._
import akka.actor._
import akka.util.ByteString
import RLPxConnectionHandler.MessageReceived
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63.GetNodeData

import scala.collection.immutable.HashSet

/**
  * Peer actor is responsible for initiating and handling high-level connection with peer.
  * It creates child RLPxConnectionActor for handling underlying RLPx communication.
  * Once RLPx connection is established it proceeds with protocol handshake (i.e `Hello` exchange).
  * Once that's done it can send/receive messages with peer (HandshakedHandler.receive).
  */
class PeerActor(nodeInfo: NodeInfo) extends Actor with ActorLogging {

  import PeerActor._
  import context.{dispatcher, system}

  val P2pVersion = 4

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

  def waitingForConnectionResult(rlpxConnection: ActorRef): Receive = handleTerminated orElse {
    case RLPxConnectionHandler.ConnectionEstablished =>
      log.info("RLPx connection established, sending Hello")
      val hello = Hello(
        p2pVersion = P2pVersion,
        clientId = "etc-client",
        capabilities = Seq(Capability("eth", Message.PV63.toByte)),
        listenPort = nodeInfo.listenAddress.getPort,
        nodeId = ByteString(nodeInfo.nodeId))
      rlpxConnection ! RLPxConnectionHandler.SendMessage(hello)
      val timeout = system.scheduler.scheduleOnce(3.seconds, self, ProtocolHandshakeTimeout)
      context become waitingForHello(rlpxConnection, timeout)

    case RLPxConnectionHandler.ConnectionFailed =>
      log.info("Cannot establish RLPx connection")
      context stop self
  }

  def waitingForHello(rlpxConnection: ActorRef, timeout: Cancellable): Receive = handleTerminated orElse {
    case MessageReceived(d: Disconnect) =>
      log.info("Received {}. Closing connection", d)
      context stop self

    case MessageReceived(hello: Hello) =>
      log.info("Received {}. Protocol handshake finished", hello)
      timeout.cancel()

      if (hello.capabilities.contains(Capability("eth", Message.PV63.toByte))) {

        rlpxConnection ! RLPxConnectionHandler.SendMessage(
          CommonMessages.Status(
            protocolVersion = Message.PV63,
            networkId = 1,
            totalDifficulty = new BigInteger("17179869184"),
            bestHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3")),
            genesisHash = ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"))))

        context become new HandshakedHandler(rlpxConnection).receive
      } else {
        log.info("Peer does not support eth {} protocol. Disconnecting.", Message.PV63.toByte)
        rlpxConnection ! SendMessage(Disconnect(Disconnect.Reasons.IncompatibleP2pProtocolVersion))
        context.system.scheduler.scheduleOnce(5.seconds, self, PoisonPill)
      }
  }

  def handleTerminated: Receive = {
    case _: Terminated =>
      log.info("RLPx connection actor terminated")
      context stop self
  }

  class HandshakedHandler(rlpxConnection: ActorRef) {

    var tomatoMap = HashSet[ActorRef]()

    def receive: Receive = handleTerminated orElse {
      case RLPxConnectionHandler.MessageReceived(message) => processMessage(message)
      case s: SendMessage[_] => sendMessage(s.message)(s.enc)
      case StartFastSync(hash) =>
        val fastSync = context.actorOf(FastSyncActor.props(self), "fast-sync")
        fastSync ! FastSyncActor.StartSync(hash)
    }

    def sendMessage[M <: Message : RLPEncoder](message: M): Unit = {
      rlpxConnection ! RLPxConnectionHandler.SendMessage(message)
      tomatoMap = tomatoMap + sender()
    }

    def processMessage(message: Message): Unit = message match {
//      /*TODO delete test code*/
//      case m:GetBlockHeaders =>
//        sendMessage(BlockHeaders(Seq()))
//        sendMessage(GetNodeData(Seq(
//          ByteString(Hex.decode("ac65a995f80381c8b0a2993b08cbee9cfc1cc2d164c33288d99a9c1a02c9e9c3"))
//          , ByteString(Hex.decode("45df225cffcb97928010c49116e7d767e09333da60644b807c512bac27ac8890"))
//          , ByteString(Hex.decode("8ccbec7896a8cc26429830406d14080a7234ea14336370bf850526e614b1b8ed"))
//        )))
//      /**/
      case Ping() =>
        sendMessage(Pong())

      case d: Disconnect =>
        log.info("Received {}. Closing connection", d)
        context stop self

      case msg =>
        tomatoMap.foreach(actor => actor ! msg)
        log.info("Received message: {}", msg)
    }

  }
}

object PeerActor {
  def props(nodeInfo: NodeInfo): Props =
    Props(new PeerActor(nodeInfo))

  case class HandleConnection(connection: ActorRef)
  case class ConnectTo(uri: URI)

  case class SendMessage[M <: Message](message: M)(implicit val enc: RLPEncoder[M])

  case class StartFastSync(blockHash: ByteString)

  private case object ProtocolHandshakeTimeout
}
