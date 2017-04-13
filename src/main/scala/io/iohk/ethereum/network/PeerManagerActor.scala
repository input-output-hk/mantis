package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.util.Timeout
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.PeerActor.Status.Handshaking
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect
import io.iohk.ethereum.utils.BlockchainConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.agent.Agent
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.utils.{Config, NodeStatus}

import scala.util.{Failure, Success}

class PeerManagerActor(
    peerConfiguration: PeerConfiguration,
    peerFactory: (ActorContext, InetSocketAddress) => ActorRef,
    externalSchedulerOpt: Option[Scheduler] = None)
  extends Actor with ActorLogging with Stash {

  import akka.pattern.{ask, pipe}
  import PeerManagerActor._
  import Config.Network.Discovery._

  var peers: Map[String, Peer] = Map.empty

  private def scheduler = externalSchedulerOpt getOrElse context.system.scheduler

  scheduler.schedule(0.seconds, bootstrapNodesScanInterval, self, ScanBootstrapNodes)

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy() {
      case _ => Stop
    }

  override def receive: Receive = handleCommonMessages orElse {
    case msg: HandlePeerConnection =>
      context become tryingToConnect
      tryDiscardPeersToFreeUpLimit()
        .map(_ => (msg, Success(())))
        .recover { case ex => (msg, Failure(ex)) }
        .pipeTo(self)

    case msg: ConnectToPeer =>
      context become tryingToConnect
      tryDiscardPeersToFreeUpLimit()
        .map(_ => (msg, Success(())))
        .recover { case ex => (msg, Failure(ex)) }
        .pipeTo(self)

    case ScanBootstrapNodes =>
      val peerAddresses = peers.values.map(_.remoteAddress).toSet
      val nodesToConnect = bootstrapNodes
        .map(new URI(_))
        .filterNot(uri => peerAddresses.contains(new InetSocketAddress(uri.getHost, uri.getPort)))

      if (nodesToConnect.nonEmpty) {
        log.info("Trying to connect to {} bootstrap nodes", nodesToConnect.size)
        nodesToConnect.foreach(self ! ConnectToPeer(_))
      }
  }

  def handleCommonMessages: Receive = {
    case GetPeers =>
      getPeers().pipeTo(sender())

    case Terminated(ref) =>
      peers -= ref.path.name
  }

  def tryingToConnect: Receive = handleCommonMessages orElse {
    case _: HandlePeerConnection | _: ConnectToPeer | ScanBootstrapNodes =>
      stash()

    case (HandlePeerConnection(connection, remoteAddress), Success(_)) =>
      context.unbecome()
      val peer = createPeer(remoteAddress)
      peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
      unstashAll()

    case (HandlePeerConnection(connection, remoteAddress), Failure(_)) =>
      log.info("Maximum number of connected peers reached. Peer {} will be disconnected.", remoteAddress)
      context.unbecome()
      val peer = createPeer(remoteAddress)
      peer.ref ! PeerActor.HandleConnection(connection, remoteAddress)
      peer.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers)
      unstashAll()

    case (ConnectToPeer(uri), Success(_)) =>
      context.unbecome()
      val peer = createPeer(new InetSocketAddress(uri.getHost, uri.getPort))
      peer.ref ! PeerActor.ConnectTo(uri)
      unstashAll()

    case (ConnectToPeer(uri), Failure(_)) =>
      log.info("Maximum number of connected peers reached. Not connecting to {}", uri)
      context.unbecome()
      unstashAll()
  }

  def createPeer(addr: InetSocketAddress): Peer = {
    val ref = peerFactory(context, addr)
    context watch ref
    val peer = Peer(addr, ref)
    peers += peer.id -> peer
    peer
  }

  def getPeers(): Future[Peers] = {
    implicit val timeout = Timeout(2.seconds)

    Future.traverse(peers.values) { peer =>
      (peer.ref ? PeerActor.GetStatus)
        .mapTo[PeerActor.StatusResponse]
        .map(sr => (peer, sr.status))
    }.map(r => Peers.apply(r.toMap))
  }

  def tryDiscardPeersToFreeUpLimit(): Future[Unit] = {
    getPeers() flatMap { peers =>
      val peersToPotentiallyDisconnect = peers.peers.filter {
        case (_, Handshaking(numRetries)) if numRetries > 0 => true /* still handshaking and retried at least once */
        case (_, PeerActor.Status.Disconnected) => true /* already disconnected */
        case _ => false
      }

      if (peers.peers.size - peersToPotentiallyDisconnect.size >= peerConfiguration.maxPeers) {
        Future.failed(new RuntimeException("Too many peers"))
      } else {
        val numPeersToDisconnect = (peers.peers.size + 1 - peerConfiguration.maxPeers) max 0

        val peersToDisconnect = peersToPotentiallyDisconnect.toSeq.sortBy {
          case (_, PeerActor.Status.Disconnected) => 0
          case (_, _: Handshaking) => 1
          case _ => 2
        }.take(numPeersToDisconnect)

        peersToDisconnect.foreach { case (p, _) => p.ref ! PeerActor.DisconnectPeer(Disconnect.Reasons.TooManyPeers) }
        Future.successful(())
      }
    }
  }

}

object PeerManagerActor {
  def props(nodeStatusHolder: Agent[NodeStatus],
            peerConfiguration: PeerConfiguration,
            appStateStorage: AppStateStorage,
            blockchain: Blockchain,
            blockchainConfig: BlockchainConfig): Props =
    Props(new PeerManagerActor(peerConfiguration,
      peerFactory(nodeStatusHolder, peerConfiguration, appStateStorage, blockchain, blockchainConfig)))

  def peerFactory(nodeStatusHolder: Agent[NodeStatus],
                  peerConfiguration: PeerConfiguration,
                  appStateStorage: AppStateStorage,
                  blockchain: Blockchain,
                  blockchainConfig: BlockchainConfig): (ActorContext, InetSocketAddress) => ActorRef = {
    (ctx, addr) =>
      val id = addr.toString.filterNot(_ == '/')
      val forkResolverOpt =
        if (blockchainConfig.customGenesisFileOpt.isDefined) None
        else Some(new ForkResolver.EtcForkResolver(blockchainConfig))
      ctx.actorOf(PeerActor.props(nodeStatusHolder, peerConfiguration, appStateStorage, blockchain, forkResolverOpt), id)
  }

  trait PeerConfiguration {
    val connectRetryDelay: FiniteDuration
    val connectMaxRetries: Int
    val disconnectPoisonPillTimeout: FiniteDuration
    val waitForStatusTimeout: FiniteDuration
    val waitForChainCheckTimeout: FiniteDuration
    val fastSyncHostConfiguration: FastSyncHostConfiguration
    val maxPeers: Int
  }

  trait FastSyncHostConfiguration {
    val maxBlocksHeadersPerMessage: Int
    val maxBlocksBodiesPerMessage: Int
    val maxReceiptsPerMessage: Int
    val maxMptComponentsPerMessage: Int
  }

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case class Peer(remoteAddress: InetSocketAddress, ref: ActorRef) {
    def id: String = ref.path.name
  }

  case object GetPeers
  case class Peers(peers: Map[Peer, PeerActor.Status]) {
    def handshaked: Map[Peer, PeerActor.Status.Handshaked] =
      peers.collect { case (p, h: PeerActor.Status.Handshaked) => (p, h) }
  }

  private case object ScanBootstrapNodes
}
