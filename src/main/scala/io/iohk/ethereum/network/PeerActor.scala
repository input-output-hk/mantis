package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}
import java.util.UUID

import akka.actor._
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.network.PeerActor.Status._
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.messages.PV63._
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.network.p2p.messages.{CommonMessages => msg}
import io.iohk.ethereum.network.p2p.validators.ForkValidator
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import org.spongycastle.crypto.AsymmetricCipherKeyPair

import scala.concurrent.duration._

/**
  * Peer actor is responsible for initiating and handling high-level connection with peer.
  * It creates child RLPxConnectionActor for handling underlying RLPx communication.
  * Once RLPx connection is established it proceeds with protocol handshake (i.e `Hello`
  * and `Status` exchange).
  * Once that's done it can send/receive messages with peer (HandshakedHandler.receive).
  */
class PeerActor(
    nodeStatusHolder: Agent[NodeStatus],
    rlpxConnectionFactory: ActorContext => ActorRef,
    storage: PeerActor.Storage)
  extends Actor with ActorLogging {

  import Config.Blockchain._
  import Config.Network.Peer._
  import PeerActor._
  import context.{dispatcher, system}

  val P2pVersion = 4

  val peerId: String = self.path.name

  val daoForkValidator = ForkValidator(daoForkBlockNumber, daoForkBlockHash)

  private var messageSubscribers: Seq[Subscriber] = Nil

  override def receive: Receive = waitingForInitialCommand

  def waitingForInitialCommand: Receive = handleSubscriptions orElse {
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

  def waitingForConnectionResult(rlpxConnection: RLPxConnection, noRetries: Int = 0): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse {
    case RLPxConnectionHandler.ConnectionEstablished =>
      log.info("RLPx connection established, sending Hello")
      rlpxConnection.sendMessage(createHelloMsg())
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

  private def scheduleConnectRetry(uri: URI, noRetries: Int): Unit = {
    log.info("Scheduling connection retry in {}", connectRetryDelay)
    system.scheduler.scheduleOnce(connectRetryDelay, self, RetryConnectionTimeout)
    context become {
      case RetryConnectionTimeout => reconnect(uri, noRetries + 1)
      case GetStatus => sender() ! StatusResponse(Connecting)
    }
  }

  def waitingForHello(rlpxConnection: RLPxConnection, timeout: Cancellable): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse {
    case RLPxConnectionHandler.MessageReceived(hello: Hello) =>
      log.info("Protocol handshake finished with peer ({})", hello)
      timeout.cancel()
      if (hello.capabilities.contains(Capability("eth", Message.PV63.toByte))) {
        rlpxConnection.sendMessage(createStatusMsg())
        val statusTimeout = system.scheduler.scheduleOnce(waitForStatusTimeout, self, StatusReceiveTimeout)
        context become waitingForNodeStatus(rlpxConnection, statusTimeout)
      } else {
        log.info("Connected peer does not support eth {} protocol. Disconnecting.", Message.PV63.toByte)
        disconnectFromPeer(rlpxConnection, Disconnect.Reasons.IncompatibleP2pProtocolVersion)
      }

    case ProtocolHandshakeTimeout =>
      log.warning("Timeout while waiting for Hello")
      disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking)
  }

  private def createStatusMsg(): msg.Status = {
    val nodeStatus = nodeStatusHolder()
    msg.Status(
      protocolVersion = Message.PV63,
      networkId = Config.Network.networkId,
      totalDifficulty = nodeStatus.blockchainStatus.totalDifficulty,
      bestHash = nodeStatus.blockchainStatus.bestHash,
      genesisHash = genesisHash)
  }

  def waitingForNodeStatus(rlpxConnection: RLPxConnection, timeout: Cancellable): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse handlePingMsg(rlpxConnection) orElse {
    case RLPxConnectionHandler.MessageReceived(status: msg.Status) =>
      timeout.cancel()
      log.info("Peer returned status ({})", status)
      rlpxConnection.sendMessage(GetBlockHeaders(Left(daoForkBlockNumber), maxHeaders = 1, skip = 0, reverse = false))
      val waitingForDaoTimeout = system.scheduler.scheduleOnce(waitForChainCheckTimeout, self, DaoHeaderReceiveTimeout)
      context become waitingForChainForkCheck(rlpxConnection, status, waitingForDaoTimeout)

    case StatusReceiveTimeout =>
      log.warning("Timeout while waiting status")
      disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking)
  }

  def waitingForChainForkCheck(rlpxConnection: RLPxConnection, remoteStatus: msg.Status, timeout: Cancellable): Receive =
    handleSubscriptions orElse handleTerminated(rlpxConnection) orElse
    handleDisconnectMsg orElse handlePingMsg(rlpxConnection) orElse handlePeerChainCheck(rlpxConnection) orElse {

    case RLPxConnectionHandler.MessageReceived(msg @ BlockHeaders(blockHeaders)) =>
      timeout.cancel()

      val daoBlockHeaderOpt = blockHeaders.find(_.number == daoForkBlockNumber)

      daoBlockHeaderOpt match {
        case Some(_) if daoForkValidator.validate(msg).isEmpty =>
          log.info("Peer is running the ETC chain")
          context become new HandshakedHandler(rlpxConnection).receive

        case Some(_) if nodeStatusHolder().blockchainStatus.totalDifficulty < daoForkBlockTotalDifficulty =>
          log.info("Peer is not running the ETC fork, but we're not there yet. Keeping the connection until then.")
          context become new HandshakedHandler(rlpxConnection).receive

        case Some(_) =>
          log.info("Peer is not running the ETC fork, disconnecting")
          disconnectFromPeer(rlpxConnection, Disconnect.Reasons.UselessPeer)

        case None if remoteStatus.totalDifficulty < daoForkBlockTotalDifficulty =>
          log.info("Peer is not at ETC fork yet. Keeping the connection until then.")
          context become new HandshakedHandler(rlpxConnection).receive

        case None =>
          log.info("Peer did not respond with ETC fork block header")
          disconnectFromPeer(rlpxConnection, Disconnect.Reasons.UselessPeer)
      }

    case DaoHeaderReceiveTimeout => disconnectFromPeer(rlpxConnection, Disconnect.Reasons.TimeoutOnReceivingAMessage)

    case GetStatus => sender() ! StatusResponse(Handshaking)
  }

  private def disconnectFromPeer(rlpxConnection: RLPxConnection, reason: Int): Unit = {
    rlpxConnection.sendMessage(Disconnect(reason))
    system.scheduler.scheduleOnce(disconnectPoisonPillTimeout, self, PoisonPill)
    context unwatch rlpxConnection.ref
    context become disconnected
  }

  def disconnected: Receive = handleSubscriptions orElse {
    case GetStatus => sender() ! StatusResponse(Disconnected)
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

  def handleEvmMptFastDownload(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(request: GetNodeData) =>
      import io.iohk.ethereum.rlp.encode

      val result: Seq[ByteString] = request.commonMptHashes.slice(0, maxMptComponentsPerMessage).flatMap { hash =>
        storage.mptNodeStorage.get(hash).map((node: MptNode) => ByteString(encode[MptNode](node)))
          .orElse(storage.evmCodeStorage.get(hash).map((evm: ByteString) => evm))
      }

      rlpxConnection.sendMessage(NodeData(result))
  }

  def handleBlockFastDownload(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(request: GetReceipts) =>
      val receipts = request.blockHashes.slice(0, maxReceiptsPerMessage)
        .flatMap(hash => storage.receiptStorage.get(hash))

      rlpxConnection.sendMessage(Receipts(receipts))

    case RLPxConnectionHandler.MessageReceived(request: GetBlockBodies) =>
      val blockBodies = request.hashes.slice(0, maxBlocksBodiesPerMessage)
        .flatMap(hash => storage.blockBodiesStorage.get(hash))

      rlpxConnection.sendMessage(BlockBodies(blockBodies))

    case RLPxConnectionHandler.MessageReceived(request: GetBlockHeaders) =>
      val blockNumber = request.block.fold(a => Some(a), b => storage.blockHeadersStorage.get(b).map(_.number))

      blockNumber match {
        case Some(startBlockNumber) if startBlockNumber >= 0 && request.maxHeaders >= 0 && request.skip >= 0 =>

          val headersCount: BigInt = if (maxBlocksHeadersPerMessage < request.maxHeaders) maxBlocksHeadersPerMessage else request.maxHeaders

          val range = if (request.reverse) {
            startBlockNumber to (startBlockNumber - (request.skip + 1) * headersCount + 1) by -(request.skip + 1)
          } else {
            startBlockNumber to (startBlockNumber + (request.skip + 1) * headersCount) by (request.skip + 1)
          }

          val blockHeaders: Seq[BlockHeader] = range.flatMap { a: BigInt => storage.blockHeadersStorage.get(a) }

          rlpxConnection.sendMessage(BlockHeaders(blockHeaders))

        case _ => log.info("got request for block headers with invalid block hash/number: {}", request)
      }
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

  def handlePeerChainCheck(rlpxConnection: RLPxConnection): Receive = {
    case RLPxConnectionHandler.MessageReceived(message@GetBlockHeaders(Left(number), _, _, _)) if number == 1920000 =>
      log.info("Received message: {}", message)
      storage.blockHeadersStorage.get(number) match {
        case Some(header) => rlpxConnection.sendMessage(BlockHeaders(Seq(header)))
        case None => rlpxConnection.sendMessage(BlockHeaders(Seq.empty))
      }
  }

  class HandshakedHandler(rlpxConnection: RLPxConnection) {

    def receive: Receive =
      handleSubscriptions orElse handleTerminated(rlpxConnection) orElse
      handlePeerChainCheck(rlpxConnection) orElse handlePingMsg(rlpxConnection) orElse
      handleBlockFastDownload(rlpxConnection) orElse handleEvmMptFastDownload(rlpxConnection) orElse {
      case RLPxConnectionHandler.MessageReceived(message) =>
        log.debug("Received message: {}", message)
        notifySubscribers(message)
        processMessage(message)

      case s: SendMessage[_] =>
        rlpxConnection.sendMessage(s.message)(s.enc)

      case GetStatus =>
        sender() ! StatusResponse(Handshaked)
    }

    def notifySubscribers(message: Message): Unit = {
      val subscribers = messageSubscribers.filter(_.messageCodes.contains(message.code))
      val toNotify = subscribers.map(_.ref).toSet
      toNotify.foreach { _ ! MessageReceived(message) }
    }

    def processMessage(message: Message): Unit = message match {
      case d: Disconnect =>
        log.info("Received {}. Closing connection", d)
        context stop self

      case msg @ BlockHeaders(blockHeaders) =>
        val daoBlockHeaderOpt = blockHeaders.find(_.number == daoForkBlockNumber)

        daoBlockHeaderOpt foreach { daoBlockHeader =>
          log.info("Reached the ETC fork block header")
          if (daoForkValidator.validate(msg).nonEmpty) {
            log.info("Peer is not running the ETC fork, disconnecting")
            disconnectFromPeer(rlpxConnection, Disconnect.Reasons.UselessPeer)
          }
        }

      case _ => // nothing
    }

  }

}

object PeerActor {
  def props(nodeStatusHolder: Agent[NodeStatus], storage: PeerActor.Storage): Props =
    Props(new PeerActor(nodeStatusHolder, rlpxConnectionFactory(nodeStatusHolder().key), storage))

  def rlpxConnectionFactory(nodeKey: AsymmetricCipherKeyPair): ActorContext => ActorRef = { ctx =>
    ctx.actorOf(RLPxConnectionHandler.props(nodeKey), "rlpx-connection")
  }

  case class RLPxConnection(ref: ActorRef, remoteAddress: InetSocketAddress, uriOpt: Option[URI]) {
    def sendMessage[M <: Message : RLPEncoder](message: M): Unit = {
      ref ! RLPxConnectionHandler.SendMessage(message)
    }
  }

  case class Storage(
    blockHeadersStorage: BlockHeadersStorage,
    blockBodiesStorage: BlockBodiesStorage,
    receiptStorage: ReceiptStorage,
    mptNodeStorage: MptNodeStorage,
    evmCodeStorage: EvmCodeStorage
  )

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

  case class Subscribe(messageCodes: Set[Int])
  case object Unsubscribe

  private case class Subscriber(ref: ActorRef, messageCodes: Set[Int])

  case class MessageReceived(message: Message)
}
