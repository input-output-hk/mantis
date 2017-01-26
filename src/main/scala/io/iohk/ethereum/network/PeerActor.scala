package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.actor._
import akka.util.ByteString
import io.iohk.ethereum.network.PeerActor.Status._
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.{CommonMessages => msg}
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

  val connectMaxRetries = 3
  val connectRetryDelay = 20.seconds

  //FIXME move this to props
  lazy val DaoBlockNumber = 1920000
  lazy val DaoBlockTotalDifficulty = BigInt("39490964433395682584")
  lazy val daoForkValidator = ForkValidator(DaoBlockNumber, ByteString(Hex.decode("94365e3a8c0b35089c1d1195081fe7489b528a84b22199c916180db8b28ade7f")))

  override def receive: Receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = {
    case HandleConnection(connection, remoteAddress) =>
      val rlpxConnection = createRlpxConnection(remoteAddress, None)
      rlpxConnection.ref ! RLPxConnectionHandler.HandleConnection(connection)
      context become waitingForConnectionResult(rlpxConnection)

    case ConnectTo(uri) =>
      val rlpxConnection = createRlpxConnection(new InetSocketAddress(uri.getHost, uri.getPort), Some(uri))
      rlpxConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
      context become waitingForConnectionResult(rlpxConnection)

    case GetStatus => sender() ! StatusResponse(Idle)
  }

  def createRlpxConnection(remoteAddress: InetSocketAddress, uriOpt: Option[URI]): RLPxConnection = {
    val ref = context.actorOf(RLPxConnectionHandler.props(nodeInfo), "rlpx-connection")
    context watch ref
    RLPxConnection(ref, remoteAddress, uriOpt)
  }

  def waitingForConnectionResult(rlpxConnection: RLPxConnection, noRetries: Int = 0): Receive = handleTerminated(rlpxConnection) orElse {
    case RLPxConnectionHandler.ConnectionEstablished =>
      log.info("RLPx connection established, sending Hello")
      val hello = Hello(
        p2pVersion = P2pVersion,
        clientId = "etc-client",
        capabilities = Seq(Capability("eth", Message.PV63.toByte)),
        listenPort = nodeInfo.listenAddress.getPort,
        nodeId = ByteString(nodeInfo.nodeId))
      rlpxConnection.sendMessage(hello)
      val timeout = system.scheduler.scheduleOnce(3.seconds, self, ProtocolHandshakeTimeout)
      context become waitingForHello(rlpxConnection, timeout)

    case RLPxConnectionHandler.ConnectionFailed =>
      log.info("Failed to establish RLPx connection")
      rlpxConnection.uriOpt match {
        case Some(uri) if noRetries < connectMaxRetries =>
          context unwatch rlpxConnection.ref
          scheduleConnectRetry(uri, noRetries)
        case Some(uri) =>
          log.info("No more reconnect attempts left, removing peer")
          context stop self
        case None =>
          log.info("Connection was initiated by remote peer, not attempting to reconnect")
          context stop self
      }

    case GetStatus => sender() ! StatusResponse(Connecting)
  }

  private def scheduleConnectRetry(uri: URI, noRetries: Int): Unit = {
    log.info("Scheduling connection retry in {}", connectRetryDelay)
    system.scheduler.scheduleOnce(connectRetryDelay, self, RetryConnectionTimeout)
    context become {
      case RetryConnectionTimeout => reconnect(uri, noRetries + 1)
      case GetStatus => sender() ! StatusResponse(Connecting)
    }
  }

  def waitingForHello(rlpxConnection: RLPxConnection, timeout: Cancellable): Receive = handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse {
    case MessageReceived(hello: Hello) =>
      log.info("Protocol handshake finished with peer ({})", hello)
      timeout.cancel()
      if (hello.capabilities.contains(Capability("eth", Message.PV63.toByte))) {
        rlpxConnection.sendMessage(ourStatus) // atm we will send the same message
        val statusTimeout = system.scheduler.scheduleOnce(waitForStatusInterval, self, StatusReceiveTimeout)
        context become waitingForNodeStatus(rlpxConnection, statusTimeout)
      } else {
        log.info("Connected peer does not support eth {} protocol. Disconnecting.", Message.PV63.toByte)
        disconnectFromPeer(rlpxConnection, Disconnect.Reasons.IncompatibleP2pProtocolVersion)
      }

    case GetStatus => sender() ! StatusResponse(Handshaking)
  }

  def waitingForNodeStatus(rlpxConnection: RLPxConnection, timeout: Cancellable): Receive = handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse
    handlePingMsg(rlpxConnection) orElse {
    case MessageReceived(status: msg.Status) =>
      timeout.cancel()
      log.info("Peer returned status ({})", status)
      rlpxConnection.sendMessage(GetBlockHeaders(Left(DaoBlockNumber), maxHeaders = 1, skip = 0, reverse = 0))
      val waitingForDaoTimeout = system.scheduler.scheduleOnce(waitForChainCheck, self, DaoHeaderReceiveTimeout)
      context become waitingForChainForkCheck(rlpxConnection, status, waitingForDaoTimeout)

    case StatusReceiveTimeout =>
      log.warning("Timeout while waiting status")
      disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking)
  }

  def waitingForChainForkCheck(rlpxConnection: RLPxConnection, status: msg.Status, timeout: Cancellable): Receive = handleTerminated(rlpxConnection) orElse
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

    case MessageReceived(BlockHeaders(Nil)) =>
      // FIXME We need to do some checking related to our blockchain. If we haven't arrived to the DAO block we might
      // take advantage of this peer and grab as much blocks as we can until DAO.
      // ATM we will only check by DaoBlockTotalDifficulty
      if (status.totalDifficulty < DaoBlockTotalDifficulty) {
        log.info("Peer probably hasn't arrived to DAO yet")
        context become new HandshakedHandler(rlpxConnection).receive
      }

    case DaoHeaderReceiveTimeout => disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking)
  }

  private def disconnectFromPeer(rlpxConnection: RLPxConnection, reason: Int): Unit = {
    rlpxConnection.sendMessage(Disconnect(Disconnect.Reasons.TimeoutOnReceivingAMessage))
    system.scheduler.scheduleOnce(5.seconds, self, PoisonPill)
    context unwatch rlpxConnection.ref
    context become disconnected
  }

  def disconnected: Receive = {
    case GetStatus => sender() ! StatusResponse(Disconnected)
  }

  // FIXME This is a temporal function until we start keeping our status
  private def ourStatus: msg.Status = {
    val totalDifficulty = 34351349760L
    msg.Status(
      protocolVersion = Message.PV63, 1, totalDifficulty,
      ByteString(Hex.decode("88e96d4537bea4d9c05d12549907b32561d3bf31f45aae734cdc119f13406cb6")),
      ByteString(Hex.decode("d4e56740f876aef8c010b86a40d5f56745a118d0906a34e69aec8c0db1cb8fa3"))
    )
  }

  def handleTerminated(rlpxConnection: RLPxConnection): Receive = {
    case _: Terminated =>
      log.info("Connection closed unexpectedly")
      rlpxConnection.uriOpt match {
        case Some(uri) => reconnect(uri, noRetries = 0)
        case None => context stop self
      }
  }

  def reconnect(uri: URI, noRetries: Int): Unit = {
    log.info("Trying to reconnect")
    val address = new InetSocketAddress(uri.getHost, uri.getPort)
    val newConnection = createRlpxConnection(address, Some(uri))
    newConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
    context become waitingForConnectionResult(newConnection, noRetries)
  }

  def handlePingMsg(rlpxConnection: RLPxConnection): Receive = {
    case MessageReceived(ping: Ping) => rlpxConnection.sendMessage(Pong())
  }

  def handleDisconnectMsg : Receive = {
    case MessageReceived(d: Disconnect) =>
      log.info("Received {}. Closing connection", d)
      context stop self
  }

  class HandshakedHandler(rlpxConnection: RLPxConnection) {

    def receive: Receive = handleTerminated(rlpxConnection) orElse {
      case RLPxConnectionHandler.MessageReceived(message) => processMessage(message)
      case s: SendMessage[_] => rlpxConnection.sendMessage(s.message)(s.enc)
      case GetStatus => sender() ! StatusResponse(Handshaked)
    }

    def processMessage(message: Message): Unit = message match {
      case Ping() =>
        rlpxConnection.sendMessage(Pong())

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

  case class RLPxConnection(ref: ActorRef, remoteAddress: InetSocketAddress, uriOpt: Option[URI]) {
    def sendMessage[M <: Message : RLPEncoder](message: M): Unit = {
      ref ! RLPxConnectionHandler.SendMessage(message)
    }
  }

  case class HandleConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectTo(uri: URI)

  case class SendMessage[M <: Message](message: M)(implicit val enc: RLPEncoder[M])

  private case object DaoHeaderReceiveTimeout

  private case object ProtocolHandshakeTimeout

  private case object StatusReceiveTimeout

  private case object RetryConnectionTimeout

  case object GetStatus
  case class StatusResponse(status: Status)

  sealed trait Status
  object Status {
    case object Idle extends Status
    case object Connecting extends Status
    case object Handshaking extends Status
    case object Handshaked extends Status
    case object Disconnected extends Status
  }

}
