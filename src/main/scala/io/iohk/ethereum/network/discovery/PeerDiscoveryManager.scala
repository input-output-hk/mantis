package io.iohk.ethereum.network.discovery

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.pipe
import akka.util.ByteString
import cats.implicits._
import cats.effect.{Resource, ExitCase}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import io.iohk.scalanet.discovery.crypto.{PrivateKey, PublicKey}
import io.iohk.scalanet.discovery.ethereum.{Node => ENode}
import io.iohk.scalanet.discovery.ethereum.v4
import java.util.concurrent.atomic.AtomicReference
import java.net.InetSocketAddress
import monix.eval.Task
import monix.execution.Scheduler
import scodec.bits.BitVector
import io.iohk.ethereum.network.discovery.codecs.RLPCodecs
import io.iohk.scalanet.peergroup.udp.StaticUDPPeerGroup
import io.iohk.scalanet.peergroup.InetMultiAddress
import io.iohk.ethereum.utils.LazyLogger
import java.net.InetAddress

class PeerDiscoveryManager(
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    // The manager only starts the DiscoveryService if discovery is enabled.
    discoveryServiceR: Resource[Task, v4.DiscoveryService]
)(implicit scheduler: Scheduler)
    extends Actor
    with ActorLogging {

  import PeerDiscoveryManager._

  // The following logic is for backwards compatibility.
  val alreadyDiscoveredNodes: Set[Node] =
    if (!discoveryConfig.reuseKnownNodes) Set.empty
    else {
      // The manager considered the bootstrap nodes discovered, even if discovery was disabled.
      val bootstrapNodes: Set[Node] =
        discoveryConfig.bootstrapNodes
      // The known nodes were considered discovered even if they haven't yet responded to pings; unless discovery was disabled.
      val knownNodes: Set[Node] =
        if (!discoveryConfig.discoveryEnabled) Set.empty
        else
          knownNodesStorage.getKnownNodes().map(Node.fromUri)

      bootstrapNodes ++ knownNodes
    }

  override def receive = init

  // The service hasn't been started yet, so it just serves the static known nodes.
  def init: Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(None, sender)

    case Start =>
      if (discoveryConfig.discoveryEnabled) {
        log.info("Starting peer discovery...")
        discoveryServiceR.allocated
          .map { case (discoveryService, release) =>
            Started(discoveryService, release)
          }
          .runToFuture
          .pipeTo(self)

        context.become(starting)
      } else {
        log.info("Peer discovery is disabled.")
      }

    case Stop =>
  }

  // Waiting for the DiscoveryService to be initialized. Keep serving known nodes.
  // This would not be needed if Actors were treated as resources themselves.
  def starting: Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(None, sender)

    case Start =>

    case Stop =>
      log.info("Stopping peer discovery...")
      context.become(stopping)

    case Started(service, release) =>
      log.info("Peer discovery started.")
      context.become(started(service, release))
  }

  // DiscoveryService started, we can ask it for nodes now.
  def started(service: v4.DiscoveryService, release: Task[Unit]): Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(Some(service), sender)

    case Start =>

    case Stop =>
      log.info("Stopping peer discovery...")
      release.as(Stopped).runToFuture.pipeTo(self)
      context.become(stopping)
  }

  // Waiting for the DiscoveryService to be initialized OR we received a stop request
  // before it even got a chance to start, so we'll stop it immediately.
  def stopping: Receive = {
    case GetDiscoveredNodesInfo =>
      sendDiscoveredNodesInfo(None, sender)

    case Start | Stop =>

    case Started(_, release) =>
      release.as(Stopped).runToFuture.pipeTo(self)

    case Stopped =>
      log.info("Peer discovery stopped.")
      context.become(init)
  }

  def sendDiscoveredNodesInfo(
      maybeDiscoveryService: Option[v4.DiscoveryService],
      recipient: ActorRef
  ): Unit = {

    val maybeDiscoveredNodes: Task[Set[Node]] =
      maybeDiscoveryService.fold(Task.pure(Set.empty[Node])) {
        _.getNodes.map { nodes =>
          nodes.map { node =>
            Node(
              id = ByteString(node.id.toByteArray),
              addr = node.address.ip,
              tcpPort = node.address.tcpPort,
              udpPort = node.address.udpPort
            )
          }
        }
      }

    maybeDiscoveredNodes
      .map(_ ++ alreadyDiscoveredNodes)
      .map(DiscoveredNodesInfo(_))
      .runToFuture
      .pipeTo(recipient)
  }
}

object PeerDiscoveryManager extends LazyLogger {
  def props(
      discoveryConfig: DiscoveryConfig,
      tcpPort: Int,
      knownNodesStorage: KnownNodesStorage,
      nodeStatusHolder: AtomicReference[NodeStatus]
  )(implicit scheduler: Scheduler): Props =
    Props(
      new PeerDiscoveryManager(
        discoveryConfig,
        knownNodesStorage,
        discoveryServiceResource(discoveryConfig, tcpPort, nodeStatusHolder)
      )
    )

  def discoveryServiceResource(
      discoveryConfig: DiscoveryConfig,
      tcpPort: Int,
      nodeStatusHolder: AtomicReference[NodeStatus]
  )(implicit scheduler: Scheduler): Resource[Task, v4.DiscoveryService] = {

    implicit val sigalg = new Secp256k1SigAlg()
    val keyPair = nodeStatusHolder.get.key
    val (privateKeyBytes, _) = crypto.keyPairToByteArrays(keyPair)
    val privateKey = PrivateKey(BitVector(privateKeyBytes))

    implicit val packetCodec = v4.Packet.packetCodec(allowDecodeOverMaxPacketSize = true)
    implicit val payloadCodec = RLPCodecs.payloadCodec
    implicit val enrContentCodec = RLPCodecs.codecFromRLPCodec(RLPCodecs.enrContentRLPCodec)

    val config = v4.DiscoveryConfig.default.copy(
      messageExpiration = discoveryConfig.messageExpiration,
      maxClockDrift = discoveryConfig.maxClockDrift,
      discoveryPeriod = discoveryConfig.scanInterval,
      requestTimeout = discoveryConfig.requestTimeout,
      kademliaTimeout = discoveryConfig.kademliaTimeout,
      kademliaBucketSize = discoveryConfig.kademliaBucketSize,
      kademliaAlpha = discoveryConfig.kademliaAlpha,
      // Discovery is going to enroll with all the bootstrap nodes.
      // In theory we could pass the known nodes as well which Mantis
      // persisted, but that could be a lot more, leading to prolonged
      // startup time while the enroll finishes.
      knownPeers = (discoveryConfig.bootstrapNodes).map { node =>
        ENode(
          id = PublicKey(BitVector(node.id.toArray[Byte])),
          address = ENode.Address(
            ip = node.addr,
            udpPort = node.udpPort,
            tcpPort = node.tcpPort
          )
        )
      }
    )

    val resource = for {
      host <- Resource.liftF {
        if (discoveryConfig.host.nonEmpty)
          Task(InetAddress.getByName(discoveryConfig.host))
        else
          // TODO: Look up the external address if it's not configured.
          Task.raiseError(
            new IllegalArgumentException(
              s"Please configure the externally visible address via -Dmantis.network.discovery.host"
            )
          )
      }

      localNode = ENode(
        id = sigalg.toPublicKey(privateKey),
        address = ENode.Address(
          ip = host,
          udpPort = discoveryConfig.port,
          tcpPort = tcpPort
        )
      )

      udpConfig = StaticUDPPeerGroup.Config(
        bindAddress = new InetSocketAddress(discoveryConfig.interface, discoveryConfig.port),
        processAddress = InetMultiAddress(new InetSocketAddress(host, discoveryConfig.port)),
        channelCapacity = discoveryConfig.channelCapacity,
        receiveBufferSizeBytes = v4.Packet.MaxPacketBitsSize / 8 * 2
      )

      peerGroup <- StaticUDPPeerGroup[v4.Packet](udpConfig)

      network <- Resource.liftF {
        v4.DiscoveryNetwork[InetMultiAddress](
          peerGroup = peerGroup,
          privateKey = privateKey,
          localNodeAddress = localNode.address,
          toNodeAddress = (address: InetMultiAddress) =>
            ENode.Address(
              ip = address.inetSocketAddress.getAddress,
              udpPort = address.inetSocketAddress.getPort,
              tcpPort = 0
            ),
          config = config
        )
      }

      service <- v4.DiscoveryService[InetMultiAddress](
        privateKey = privateKey,
        node = localNode,
        config = config,
        network = network,
        toAddress = (address: ENode.Address) => InetMultiAddress(new InetSocketAddress(address.ip, address.udpPort))
      )

      _ <- Resource.liftF {
        Task {
          nodeStatusHolder.updateAndGet(
            _.copy(discoveryStatus = ServerStatus.Listening(udpConfig.bindAddress))
          )
        }
      }
    } yield service

    resource
      .onFinalizeCase {
        case ExitCase.Error(ex) =>
          Task(log.error(s"Discovery service exited with error", ex))
        case _ => Task.unit
      }
      .onFinalize {
        Task(nodeStatusHolder.updateAndGet(_.copy(discoveryStatus = ServerStatus.NotListening)))
      }
  }

  case object Start
  case object Stop

  private case class Started(service: v4.DiscoveryService, release: Task[Unit])
  private case object Stopped

  case object GetDiscoveredNodesInfo
  case class DiscoveredNodesInfo(nodes: Set[Node])
}
