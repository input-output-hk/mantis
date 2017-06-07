package io.iohk.ethereum.network

import java.net.{InetSocketAddress, URI}

import akka.util.Timeout
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.PeerActor.Status.Handshaking
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import akka.actor.SupervisorStrategy.Stop
import akka.actor.{ActorRef, _}
import akka.agent.Agent
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.network.EtcMessageHandler.EtcPeerInfo
import io.iohk.ethereum.network.PeerActor.DisconnectPeer
import io.iohk.ethereum.network.PeerEventBusActor.{PeerEvent, Publish}
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.utils.{Config, NodeStatus}

import scala.util.{Failure, Success}

class PeerManagerActor(
    peerConfiguration: PeerConfiguration,
    peerEventBus: ActorRef,
    peerFactory: (ActorContext, InetSocketAddress) => ActorRef,
    externalSchedulerOpt: Option[Scheduler] = None,
    bootstrapNodes: Set[String] = Config.Network.Discovery.bootstrapNodes)
  extends Actor with ActorLogging with Stash {

  import akka.pattern.{ask, pipe}
  import PeerManagerActor._
  import Config.Network.Discovery._

  var peers: Map[PeerId, PeerActorWithAddress] = Map.empty

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
      val currentPeers: Future[Peers] = getPeers().map( peersWithStatus =>
        //FIXME: The mapping from PeerActorWithAddress to PeerImpl should be done in the Network trait
        Peers(peersWithStatus.map{ case (peer, status) => new PeerImpl(peer.ref, peerEventBus) -> status })
      )
      currentPeers.pipeTo(sender())

    case Terminated(ref) =>
      val peerIdOpt = peers.collect{ case (id, peer) if peer.ref == ref => id }
      peerIdOpt.foreach{ peerId =>
        peers -= peerId
        peerEventBus ! Publish(PeerEvent.PeerDisconnected(peerId))
      }
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

  def createPeer(addr: InetSocketAddress): PeerActorWithAddress = {
    val ref = peerFactory(context, addr)
    context watch ref
    val peer = PeerActorWithAddress(addr, ref)
    peers += PeerId(ref.path.name) -> peer
    peer
  }

  def getPeers(): Future[Map[PeerActorWithAddress, PeerActor.Status]] = {
    implicit val timeout = Timeout(2.seconds)

    Future.traverse(peers.values) { peer =>
      (peer.ref ? PeerActor.GetStatus)
        .mapTo[PeerActor.StatusResponse]
        .map(sr => (peer, sr.status))
    }.map(_.toMap)
  }

  def tryDiscardPeersToFreeUpLimit(): Future[Unit] = {
    getPeers() flatMap { peers =>
      val peersToPotentiallyDisconnect = peers.filter {
        case (_, Handshaking(numRetries)) if numRetries > 0 => true /* still handshaking and retried at least once */
        case (_, PeerActor.Status.Disconnected) => true /* already disconnected */
        case _ => false
      }

      if (peers.size - peersToPotentiallyDisconnect.size >= peerConfiguration.maxPeers) {
        Future.failed(new RuntimeException("Too many peers"))
      } else {
        val numPeersToDisconnect = (peers.size + 1 - peerConfiguration.maxPeers) max 0

        val peersToDisconnect = peersToPotentiallyDisconnect.toSeq.sortBy {
          case (_, PeerActor.Status.Disconnected) => 0
          case (_, _: Handshaking) => 1
          case _ => 2
        }.take(numPeersToDisconnect)

        peersToDisconnect.foreach { case (p, _) => p.ref ! DisconnectPeer(Disconnect.Reasons.TooManyPeers) }
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
      bootstrapNodes: Set[String] = Config.Network.Discovery.bootstrapNodes,
      peerEventBus: ActorRef,
      forkResolverOpt: Option[ForkResolver],
      handshaker: Handshaker[EtcPeerInfo]): Props = {
    Props(new PeerManagerActor(peerConfiguration, peerEventBus,
      peerFactory = peerFactory(nodeStatusHolder, peerConfiguration, appStateStorage, blockchain, peerEventBus,
        forkResolverOpt, handshaker),
      bootstrapNodes = bootstrapNodes)
    )
  }

  def peerFactory(nodeStatusHolder: Agent[NodeStatus],
                  peerConfiguration: PeerConfiguration,
                  appStateStorage: AppStateStorage,
                  blockchain: Blockchain,
                  peerEventBus: ActorRef,
                  forkResolverOpt: Option[ForkResolver],
                  handshaker: Handshaker[EtcPeerInfo]): (ActorContext, InetSocketAddress) => ActorRef = {
    (ctx, addr) =>
      val id = addr.toString.filterNot(_ == '/')
      //FIXME: Message handler builder should be configurable
      val messageHandlerBuilder: (EtcPeerInfo, Peer) => MessageHandler[EtcPeerInfo, EtcPeerInfo] =
        EtcMessageHandler.etcMessageHandlerBuilder(forkResolverOpt, appStateStorage, peerConfiguration, blockchain)
      ctx.actorOf(PeerActor.props(nodeStatusHolder, peerConfiguration, peerEventBus,
        handshaker, messageHandlerBuilder), id)
  }

  trait PeerConfiguration {
    val connectRetryDelay: FiniteDuration
    val connectMaxRetries: Int
    val disconnectPoisonPillTimeout: FiniteDuration
    val waitForHelloTimeout: FiniteDuration
    val waitForStatusTimeout: FiniteDuration
    val waitForChainCheckTimeout: FiniteDuration
    val fastSyncHostConfiguration: FastSyncHostConfiguration
    val maxPeers: Int
    val networkId: Int
  }

  trait FastSyncHostConfiguration {
    val maxBlocksHeadersPerMessage: Int
    val maxBlocksBodiesPerMessage: Int
    val maxReceiptsPerMessage: Int
    val maxMptComponentsPerMessage: Int
  }

  case class HandlePeerConnection(connection: ActorRef, remoteAddress: InetSocketAddress)

  case class ConnectToPeer(uri: URI)

  case object GetPeers
  case class Peers(peers: Map[Peer, PeerActor.Status]) {
    def handshaked: Map[Peer, PeerActor.Status.Handshaked] =
      peers.collect { case (p, h: PeerActor.Status.Handshaked) => (p, h) }
  }

  private case object ScanBootstrapNodes

  case class PeerActorWithAddress(remoteAddress: InetSocketAddress, ref: ActorRef)

}
