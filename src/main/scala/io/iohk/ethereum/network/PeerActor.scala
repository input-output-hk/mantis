package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import io.iohk.ethereum.network.PeerManagerActor.{Peer, PeerConfiguration}

import scala.concurrent.duration._
import akka.actor._
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.PeerActor.Status._
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockHeaders, GetBlockHeaders, _}
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.messages.{CommonMessages => msg}
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.MessageHandler.MessageAction.TransmitMessage
import io.iohk.ethereum.network.MessageHandler.MessageHandlingResult
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import org.spongycastle.crypto.AsymmetricCipherKeyPair

/**
  * Peer actor is responsible for initiating and handling high-level connection with peer.
  * It creates child RLPxConnectionActor for handling underlying RLPx communication.
  * Once RLPx connection is established it proceeds with protocol handshake (i.e `Hello`
  * and `Status` exchange).
  * Once that's done it can send/receive messages with peer (HandshakedHandler.receive).
  */
//FIXME: MessageHandler type should be configurable, this is dependant on PR 185
class PeerActor(
    peerAddress: InetSocketAddress,
    nodeStatusHolder: Agent[NodeStatus],
    rlpxConnectionFactory: ActorContext => ActorRef,
    val peerConfiguration: PeerConfiguration,
    appStateStorage: AppStateStorage,
    val blockchain: Blockchain,
    externalSchedulerOpt: Option[Scheduler] = None,
    forkResolverOpt: Option[ForkResolver],
    messageHandlerBuilder: (EtcPeerInfo, Peer) => MessageHandler[EtcPeerInfo, EtcPeerInfo])
  extends Actor with ActorLogging with Stash {

  import PeerActor._
  import context.{dispatcher, system}

  def scheduler: Scheduler = externalSchedulerOpt getOrElse system.scheduler

  val P2pVersion = 4

  val peerId: String = self.path.name
  val peer: Peer = Peer(peerAddress, self)

  private var messageSubscribers: Seq[Subscriber] = Nil

  override def receive: Receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = handleSubscriptions orElse stashMessages orElse {
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
    val ref = rlpxConnectionFactory(context)
    context watch ref
    RLPxConnection(ref, remoteAddress, uriOpt)
  }

  def waitingForConnectionResult(rlpxConnection: RLPxConnection, numRetries: Int = 0): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse stashMessages orElse {
    case RLPxConnectionHandler.ConnectionEstablished =>
      log.info("RLPx connection established, sending Hello")
      rlpxConnection.sendMessage(createHelloMsg())
      val timeout = scheduler.scheduleOnce(3.seconds, self, ProtocolHandshakeTimeout)
      context become waitingForHello(rlpxConnection, timeout, numRetries)

    case RLPxConnectionHandler.ConnectionFailed =>
      log.warning("Failed to establish RLPx connection")
      rlpxConnection.uriOpt match {
        case Some(uri) if numRetries < peerConfiguration.connectMaxRetries =>
          context unwatch rlpxConnection.ref
          scheduleConnectRetry(uri, numRetries)
        case Some(uri) =>
          log.warning("No more reconnect attempts left, removing peer")
          context stop self
        case None =>
          log.warning("Connection was initiated by remote peer, not attempting to reconnect")
          context stop self
      }

    case GetStatus => sender() ! StatusResponse(Connecting)
  }

  private def createHelloMsg(): Hello = {
    val nodeStatus = nodeStatusHolder()
    val listenPort = nodeStatus.serverStatus match {
      case ServerStatus.Listening(address) => address.getPort
      case ServerStatus.NotListening => 0
    }
    Hello(
      p2pVersion = P2pVersion,
      clientId = Config.clientId,
      capabilities = Seq(Capability("eth", Message.PV63.toByte)),
      listenPort = listenPort,
      nodeId = ByteString(nodeStatus.nodeId))
  }

  private def scheduleConnectRetry(uri: URI, numRetries: Int): Unit = {
    log.info("Scheduling connection retry in {}", peerConfiguration.connectRetryDelay)
    scheduler.scheduleOnce(peerConfiguration.connectRetryDelay, self, RetryConnectionTimeout)
    context become {
      case RetryConnectionTimeout => reconnect(uri, numRetries + 1)
      case GetStatus => sender() ! StatusResponse(Connecting)
    }
  }

  def waitingForHello(rlpxConnection: RLPxConnection, timeout: Cancellable, numRetries: Int): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse stashMessages orElse {
    case RLPxConnectionHandler.MessageReceived(hello: Hello) =>
      log.info("Protocol handshake finished with peer ({})", hello)
      timeout.cancel()
      if (hello.capabilities.contains(Capability("eth", Message.PV63.toByte))) {
        rlpxConnection.sendMessage(createStatusMsg())
        val statusTimeout = scheduler.scheduleOnce(peerConfiguration.waitForStatusTimeout, self, StatusReceiveTimeout)
        context become waitingForNodeStatus(rlpxConnection, statusTimeout)
      } else {
        log.warning("Connected peer does not support eth {} protocol. Disconnecting.", Message.PV63.toByte)
        disconnectFromPeer(rlpxConnection, Disconnect.Reasons.IncompatibleP2pProtocolVersion)
      }

    case ProtocolHandshakeTimeout =>
      log.warning("Timeout while waiting for Hello")
      disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking(numRetries))
  }

  private def getBestBlockHeader() = {
    val bestBlockNumber = appStateStorage.getBestBlockNumber()
    blockchain.getBlockHeaderByNumber(bestBlockNumber).getOrElse(blockchain.genesisHeader)
  }

  private def createStatusMsg(): msg.Status = {
    val bestBlockHeader = getBestBlockHeader()
    msg.Status(
      protocolVersion = Message.PV63,
      networkId = peerConfiguration.networkId,
      totalDifficulty = bestBlockHeader.difficulty,
      bestHash = bestBlockHeader.hash,
      genesisHash = blockchain.genesisHeader.hash)
  }

  def waitingForNodeStatus(rlpxConnection: RLPxConnection, timeout: Cancellable): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse handlePingMsg(rlpxConnection) orElse stashMessages orElse {
    case RLPxConnectionHandler.MessageReceived(status: msg.Status) =>
      timeout.cancel()
      log.info("Peer returned status ({})", status)

      forkResolverOpt match {
        case Some(forkResolver) =>
          rlpxConnection.sendMessage(GetBlockHeaders(Left(forkResolver.forkBlockNumber), maxHeaders = 1, skip = 0, reverse = false))
          val timeout = scheduler.scheduleOnce(peerConfiguration.waitForChainCheckTimeout, self, ForkHeaderReceiveTimeout)
          context become waitingForForkHeader(rlpxConnection, status, timeout, forkResolver)
        case None =>
          startMessageHandler(rlpxConnection, status, 0, true)
      }

    case StatusReceiveTimeout =>
      log.warning("Timeout while waiting status")
      disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking(0))
  }

  def waitingForForkHeader(rlpxConnection: RLPxConnection, remoteStatus: msg.Status, timeout: Cancellable,
                           forkResolver: ForkResolver): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse handlePingMsg(rlpxConnection) orElse
    handlePeerChainCheck(rlpxConnection, forkResolver) orElse stashMessages orElse {

    case RLPxConnectionHandler.MessageReceived(msg @ BlockHeaders(blockHeaders)) =>
      timeout.cancel()

      val forkBlockHeaderOpt = blockHeaders.find(_.number == forkResolver.forkBlockNumber)

      forkBlockHeaderOpt match {
        case Some(forkBlockHeader) =>
          val fork = forkResolver.recognizeFork(forkBlockHeader)

          log.info("Peer is running the {} fork", fork)

          if (forkResolver.isAccepted(fork)) {
            log.info("Fork is accepted")
            startMessageHandler(rlpxConnection, remoteStatus, forkBlockHeader.number, true)
          } else {
            log.warning("Fork is not accepted")
            disconnectFromPeer(rlpxConnection, Disconnect.Reasons.UselessPeer)
          }

        case None =>
          log.info("Peer did not respond with fork block header")
          startMessageHandler(rlpxConnection, remoteStatus, 0, false)
      }

    case ForkHeaderReceiveTimeout => disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking(0))
  }

  private def startMessageHandler(rlpxConnection: RLPxConnection, remoteStatus: msg.Status,
                                  currentMaxBlockNumber: BigInt, forkAccepted: Boolean): Unit = {
    val peerInfo = EtcPeerInfo(remoteStatus, remoteStatus.totalDifficulty, forkAccepted, currentMaxBlockNumber)
    val messageHandler = messageHandlerBuilder(peerInfo, peer)
    context become new HandshakedPeer(rlpxConnection, messageHandler).receive
    rlpxConnection.sendMessage(GetBlockHeaders(Right(remoteStatus.bestHash), 1, 0, false))
    unstashAll()
  }

  private def disconnectFromPeer(rlpxConnection: RLPxConnection, reason: Int): Unit = {
    rlpxConnection.sendMessage(Disconnect(reason))
    scheduler.scheduleOnce(peerConfiguration.disconnectPoisonPillTimeout, self, PoisonPill)
    context unwatch rlpxConnection.ref
    context become disconnected
  }

  def disconnected: Receive = handleSubscriptions orElse {
    case GetStatus => sender() ! StatusResponse(Disconnected)
  }

  def handleTerminated(rlpxConnection: RLPxConnection): Receive = {
    case Terminated(actor) if actor == rlpxConnection.ref =>
      log.warning(s"Underlying rlpx connection with peer $peerId closed")
      rlpxConnection.uriOpt match {
        case Some(uri) => scheduleConnectRetry(uri, numRetries = 0)
        case None => context stop self
      }
  }

  def reconnect(uri: URI, numRetries: Int): Unit = {
    log.info("Trying to reconnect")
    val address = new InetSocketAddress(uri.getHost, uri.getPort)
    val newConnection = createRlpxConnection(address, Some(uri))
    newConnection.ref ! RLPxConnectionHandler.ConnectTo(uri)
    context become waitingForConnectionResult(newConnection, numRetries)
  }

  def handlePingMsg(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(ping: Ping) => rlpxConnection.sendMessage(Pong())
  }

  def handleDisconnectMsg: Receive = {
    case RLPxConnectionHandler.MessageReceived(d: Disconnect) =>
      log.info("Received {}. Closing connection", d)
      context stop self
  }

  def handleSubscriptions: Receive = {
    case Subscribe(messageCodes) =>
      val (senderSubscriptions, remainingSubscriptions) = messageSubscribers.partition(_.ref == sender())
      val allMessageCodes = senderSubscriptions.flatMap(_.messageCodes).toSet ++ messageCodes
      messageSubscribers = remainingSubscriptions :+ Subscriber(sender(), allMessageCodes)

    case Unsubscribe =>
      messageSubscribers = messageSubscribers.filterNot(_.ref == sender())
  }

  //FIXME: As the peer chain check is part of the handshaker, it should be incorporated into the handshake
  def handlePeerChainCheck(rlpxConnection: RLPxConnection, forkResolver: ForkResolver): Receive = {
    case RLPxConnectionHandler.MessageReceived(GetBlockHeaders(Left(number), numHeaders, _, _))
      if number == forkResolver.forkBlockNumber && numHeaders == 1 =>
      log.debug("Received request for fork block")
      blockchain.getBlockHeaderByNumber(number) match {
        case Some(header) => rlpxConnection.sendMessage(BlockHeaders(Seq(header)))
        case None => rlpxConnection.sendMessage(BlockHeaders(Nil))
      }
  }

  def stashMessages: Receive = {
    case _: SendMessage[_] | _: DisconnectPeer => stash()
  }

  class HandshakedPeer(rlpxConnection: RLPxConnection,
                       messageHandler: MessageHandler[EtcPeerInfo, EtcPeerInfo]) {

    /**
      * main behavior of actor that handles peer communication and subscriptions for messages
      */
    def receive: Receive =
      handleSubscriptions orElse
      handlePingMsg(rlpxConnection) orElse
        handleDisconnectMsg orElse
        handleTerminated(rlpxConnection) orElse {

        case RLPxConnectionHandler.MessageReceived(message) =>
          log.debug("Received message: {}", message)
          val MessageHandlingResult(newHandler, messageAction) = messageHandler.receivingMessage(message)
          if(messageAction == TransmitMessage)
            notifySubscribers(message)
          context become new HandshakedPeer(rlpxConnection, newHandler).receive

        case DisconnectPeer(reason) =>
          disconnectFromPeer(rlpxConnection, reason)

        case s@SendMessage(message) =>
          val MessageHandlingResult(newHandler, messageAction) = messageHandler.sendingMessage(message)
          if(messageAction == TransmitMessage)
            rlpxConnection.sendMessage(message)(s.enc)
          context become new HandshakedPeer(rlpxConnection, newHandler).receive

        case GetStatus =>
          sender() ! StatusResponse(
            Handshaked(messageHandler.peerInfo.remoteStatus,
              messageHandler.peerInfo.forkAccepted, messageHandler.peerInfo.totalDifficulty))

      }

    private def notifySubscribers(message: Message): Unit = {
      val subscribers = messageSubscribers.filter(_.messageCodes.contains(message.code))
      val toNotify = subscribers.map(_.ref).toSet
      toNotify.foreach { _ ! MessageReceived(message) }
    }

  }

}

object PeerActor {
  def props(peerAddress: InetSocketAddress,
            nodeStatusHolder: Agent[NodeStatus],
            peerConfiguration: PeerConfiguration,
            appStateStorage: AppStateStorage,
            blockchain: Blockchain,
            forkResolverOpt: Option[ForkResolver],
            messageHandlerBuilder: (EtcPeerInfo, Peer) => MessageHandler[EtcPeerInfo, EtcPeerInfo]): Props =
    Props(new PeerActor(
      peerAddress,
      nodeStatusHolder,
      rlpxConnectionFactory(nodeStatusHolder().key),
      peerConfiguration,
      appStateStorage,
      blockchain,
      forkResolverOpt = forkResolverOpt,
      messageHandlerBuilder = messageHandlerBuilder))

  def rlpxConnectionFactory(nodeKey: AsymmetricCipherKeyPair): ActorContext => ActorRef = { ctx =>
    ctx.actorOf(RLPxConnectionHandler.props(nodeKey), "rlpx-connection")
  }

  case class RLPxConnection(ref: ActorRef, remoteAddress: InetSocketAddress, uriOpt: Option[URI]) {
    def sendMessage[M <: Message : RLPEncoder](message: M): Unit = {
      ref ! RLPxConnectionHandler.SendMessage(message)
    }
  }

  case class HandleConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectTo(uri: URI)

  case class SendMessage[M <: Message](message: M)(implicit val enc: RLPEncoder[M])

  private case object ForkHeaderReceiveTimeout

  private case object ProtocolHandshakeTimeout

  private case object StatusReceiveTimeout

  private case object RetryConnectionTimeout

  case object GetStatus
  case class StatusResponse(status: Status)

  case class DisconnectPeer(reason: Int)

  sealed trait Status
  object Status {
    case object Idle extends Status
    case object Connecting extends Status
    case class Handshaking(numRetries: Int) extends Status
    case class Handshaked(initialStatus: msg.Status, forkAccepted: Boolean, totalDifficulty: BigInt) extends Status
    case object Disconnected extends Status
  }

  case class Subscribe(messageCodes: Set[Int])
  case object Unsubscribe

  private case class Subscriber(ref: ActorRef, messageCodes: Set[Int])

  case class MessageReceived(message: Message)
}
