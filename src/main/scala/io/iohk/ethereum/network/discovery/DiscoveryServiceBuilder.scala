package io.iohk.ethereum.network.discovery

import cats.effect.Resource
import io.iohk.ethereum.crypto
import io.iohk.ethereum.network.discovery.codecs.RLPCodecs
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import io.iohk.scalanet.discovery.crypto.{PrivateKey, PublicKey, SigAlg}
import io.iohk.scalanet.discovery.ethereum.{Node => ENode, EthereumNodeRecord}
import io.iohk.scalanet.discovery.ethereum.v4
import io.iohk.scalanet.peergroup.{InetMultiAddress, ExternalAddressResolver}
import io.iohk.scalanet.peergroup.udp.StaticUDPPeerGroup
import java.net.InetAddress
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference
import monix.eval.Task
import monix.execution.Scheduler
import scodec.bits.BitVector
import scodec.Codec

trait DiscoveryServiceBuilder {

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

    val resource = for {
      host <- Resource.liftF {
        getExternalAddress(discoveryConfig)
      }
      localNode = ENode(
        id = sigalg.toPublicKey(privateKey),
        address = ENode.Address(
          ip = host,
          udpPort = discoveryConfig.port,
          tcpPort = tcpPort
        )
      )
      config = makeDiscoveryConfig(discoveryConfig)
      udpConfig = makeUdpConfig(discoveryConfig, host)
      peerGroup <- StaticUDPPeerGroup[v4.Packet](udpConfig)
      network <- makeDiscoveryNetwork(peerGroup, privateKey, localNode, config)
      service <- makeDiscoveryService(network, privateKey, localNode, config)
      _ <- Resource.liftF {
        setDiscoveryStatus(nodeStatusHolder, ServerStatus.Listening(udpConfig.bindAddress))
      }
    } yield service

    resource
      .onFinalize {
        setDiscoveryStatus(nodeStatusHolder, ServerStatus.NotListening)
      }
  }

  private def makeDiscoveryConfig(discoveryConfig: DiscoveryConfig): v4.DiscoveryConfig =
    v4.DiscoveryConfig.default.copy(
      messageExpiration = discoveryConfig.messageExpiration,
      maxClockDrift = discoveryConfig.maxClockDrift,
      discoveryPeriod = discoveryConfig.scanInterval,
      requestTimeout = discoveryConfig.requestTimeout,
      kademliaTimeout = discoveryConfig.kademliaTimeout,
      kademliaBucketSize = discoveryConfig.kademliaBucketSize,
      kademliaAlpha = discoveryConfig.kademliaAlpha,
      // Discovery is going to enroll with all the bootstrap nodes passed to it.
      // In theory we could give it the (potentially hundreds of) nodes Mantis
      // peristed on earlier runs. Because the enrollment happens in the background,
      // it wouldn't cause any slowdown in the initialization process.
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

  private def getExternalAddress(discoveryConfig: DiscoveryConfig): Task[InetAddress] =
    discoveryConfig.host match {
      case Some(host) =>
        Task(InetAddress.getByName(host))

      case None =>
        ExternalAddressResolver.default.resolve.flatMap {
          case Some(address) =>
            Task.pure(address)
          case None =>
            Task.raiseError(
              new IllegalStateException(
                s"Failed to resolve the external address. Please configure it via -Dmantis.network.discovery.host"
              )
            )
        }
    }

  private def makeUdpConfig(discoveryConfig: DiscoveryConfig, host: InetAddress): StaticUDPPeerGroup.Config =
    StaticUDPPeerGroup.Config(
      bindAddress = new InetSocketAddress(discoveryConfig.interface, discoveryConfig.port),
      processAddress = InetMultiAddress(new InetSocketAddress(host, discoveryConfig.port)),
      channelCapacity = discoveryConfig.channelCapacity,
      receiveBufferSizeBytes = v4.Packet.MaxPacketBitsSize / 8 * 2
    )

  private def setDiscoveryStatus(nodeStatusHolder: AtomicReference[NodeStatus], status: ServerStatus): Task[Unit] =
    Task(nodeStatusHolder.updateAndGet(_.copy(discoveryStatus = status)))

  private def makeDiscoveryNetwork(
      peerGroup: StaticUDPPeerGroup[v4.Packet],
      privateKey: PrivateKey,
      localNode: ENode,
      config: v4.DiscoveryConfig
  )(implicit codec: Codec[v4.Payload], sigalg: SigAlg): Resource[Task, v4.DiscoveryNetwork[InetMultiAddress]] =
    Resource.liftF {
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

  private def makeDiscoveryService(
      network: v4.DiscoveryNetwork[InetMultiAddress],
      privateKey: PrivateKey,
      localNode: ENode,
      config: v4.DiscoveryConfig
  )(implicit sigalg: SigAlg, enrContentCodec: Codec[EthereumNodeRecord.Content]): Resource[Task, v4.DiscoveryService] =
    v4.DiscoveryService[InetMultiAddress](
      privateKey = privateKey,
      node = localNode,
      config = config,
      network = network,
      toAddress = (address: ENode.Address) => InetMultiAddress(new InetSocketAddress(address.ip, address.udpPort)),
      // On a network with many bootstrap nodes the enrollment and the initial self-lookup can take considerable
      // amount of time. We can do the enrollment in the background, which means the service is available from the
      // start, and the nodes can be contacted and gradually as they are discovered during the iterative lookup,
      // rather than at the end of the enrollment. Mantis will also contact its previously persisted peers,
      // from that perspective it doesn't care whether enrollment is over or not.
      enrollInBackground = true
    )
}
