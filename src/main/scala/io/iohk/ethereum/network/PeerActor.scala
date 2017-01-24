package io.iohk.ethereum.network

import java.net.URI

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.validators.ForkValidator
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.MessageReceived
import io.iohk.ethereum.rlp.RLPEncoder
import org.spongycastle.util.encoders.Hex

import scala.concurrent.duration._

/**
  * Peer actor is responsible for initiating and handling high-level connection with peer.
  * It creates child RLPxConnectionActor for handling underlying RLPx communication.
  * Once RLPx connection is established it proceeds with protocol handshake (i.e `Hello`
  * and `Status` exchange).
  * Once that's done it can send/receive messages with peer (HandshakedHandler.receive).
  */
class PeerActor(nodeInfo: NodeInfo) extends Actor with ActorLogging {

  import PeerActor._
  import context.{dispatcher, system}

  val P2pVersion = 4

  val peerId = self.path.name

  val waitForStatusInterval = 30.seconds
  val waitForChainCheck = 15.seconds

  //FIXME move this to props
  lazy val DaoBlockNumber = 1920000
  lazy val DaoBlockTotalDifficulty = BigInt("39490964433395682584")
  lazy val daoForkValidator = ForkValidator(DaoBlockNumber, Hex.decode("94365e3a8c0b35089c1d1195081fe7489b528a84b22199c916180db8b28ade7f"))

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

  def waitingForHello(rlpxConnection: ActorRef, timeout: Cancellable): Receive = handleTerminated orElse
    handleDisconnectMsg orElse {
    case MessageReceived(hello: Hello) =>
      log.info("Protocol handshake finished with peer ({})", hello)
      timeout.cancel()
      if (hello.capabilities.contains(Capability("eth", Message.PV63.toByte))) {
        rlpxConnection ! RLPxConnectionHandler.SendMessage(ourStatus) // atm we will send the same message
        val statusTimeout = system.scheduler.scheduleOnce(waitForStatusInterval, self, StatusReceiveTimeout)
        context become waitingForNodeStatus(rlpxConnection, statusTimeout)
      } else {
        log.info("Connected peer does not support eth {} protocol. Disconnecting.", Message.PV63.toByte)
        disconnectFromPeer(rlpxConnection, Disconnect.Reasons.IncompatibleP2pProtocolVersion)
      }
  }

  def waitingForNodeStatus(rlpxConnection: ActorRef, timeout: Cancellable): Receive = handleTerminated orElse
    handleDisconnectMsg orElse
    handlePingMsg(rlpxConnection) orElse {
    case MessageReceived(status: Status) =>
      timeout.cancel()
      log.info("Peer returned status ({})", status)
      rlpxConnection ! RLPxConnectionHandler.SendMessage(
        GetBlockHeaders(Left(DaoBlockNumber), maxHeaders = 1, skip = 0, reverse = 0)
      )
      val waitingForDaoTimeout = system.scheduler.scheduleOnce(waitForChainCheck, self, DaoHeaderReceiveTimeout)
      context become waitingForChainForkCheck(rlpxConnection, status, waitingForDaoTimeout)
    case StatusReceiveTimeout =>
      log.warning("Timeout while waiting status")
      disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)
  }

  def waitingForChainForkCheck(rlpxConnection: ActorRef, status: Status, timeout: Cancellable): Receive = handleTerminated orElse
    handleDisconnectMsg orElse
    handlePingMsg(rlpxConnection) orElse {
    case MessageReceived(msg@BlockHeaders(blockHeader +: Nil)) if blockHeader.number == DaoBlockNumber =>
      timeout.cancel()
      log.info("DAO Fork header received from peer - {}", Hex.toHexString(blockHeader.hash))
      if (daoForkValidator.validate(msg).isEmpty) {
        log.warning("Peer is running the ETC chain")
        context become new HandshakedHandler(rlpxConnection).receive
      } else {
        log.warning("Peer is not running the ETC fork, disconnecting")
        disconnectFromPeer(rlpxConnection, Disconnect.Reasons.UselessPeer)
      }
    case DaoHeaderReceiveTimeout =>
      // FIXME We need to do some checking related to our blockchain. If we haven't arrived to the DAO block we might
      // take advantage of this peer and grab as much blocks as we can until DAO.
      // ATM we will only check by DaoBlockTotalDifficulty
      if (status.totalDifficulty < DaoBlockTotalDifficulty) {
        log.info("Peer probably hasn't arrived to DAO yet")
        context become new HandshakedHandler(rlpxConnection).receive
      }
      else {
        log.info("Peer probably has arrived to DAO, so we will disconnect")
        disconnectFromPeer(rlpxConnection, Disconnect.Reasons.UselessPeer)
      }
  }

  private def disconnectFromPeer(rlpxConnection: ActorRef, reason: Int): Unit = {
    rlpxConnection ! RLPxConnectionHandler.SendMessage(Disconnect(Disconnect.Reasons.TimeoutOnReceivingAMessage))
    context.system.scheduler.scheduleOnce(5.seconds, self, PoisonPill)
  }

  // FIXME This is a temporal function until we start keeping our status
  private def ourStatus: Status = {
    val totalDifficulty = 34351349760L
    Status(
      protocolVersion = Message.PV63, 1, totalDifficulty,
      ByteString(Hex.decode("88e96d4537bea4d9c05d12549907b32561d3bf31f45aae734cdc119f13406cb6")),
      ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"))
    )
  }

  def handleTerminated: Receive = {
    case _: Terminated =>
      log.info("RLPx connection actor terminated")
      context stop self
  }

  def handlePingMsg(rlpxConnection: ActorRef): Receive = {
    case MessageReceived(ping: Ping) => rlpxConnection ! RLPxConnectionHandler.SendMessage(Pong())
  }

  def handleDisconnectMsg : Receive = {
    case MessageReceived(d: Disconnect) =>
      log.info("Received {} from peer. Closing connection", d)
      context stop self
  }

  class HandshakedHandler(rlpxConnection: ActorRef) {

    def receive: Receive = handleTerminated orElse {
      case RLPxConnectionHandler.MessageReceived(message) => processMessage(message)
      case s: SendMessage[_] => sendMessage(s.message)(s.enc)
    }

    def sendMessage[M <: Message : RLPEncoder](message: M): Unit = {
      rlpxConnection ! RLPxConnectionHandler.SendMessage(message)
    }

    def processMessage(message: Message): Unit = message match {
      case Ping() =>
        sendMessage(Pong())

      case d: Disconnect =>
        log.info("Received {}. Closing connection", d)
        context stop self

      case msg =>
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

  private case object DaoHeaderReceiveTimeout

  private case object ProtocolHandshakeTimeout

  private case object StatusReceiveTimeout

}
