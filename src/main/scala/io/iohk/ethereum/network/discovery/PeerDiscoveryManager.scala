package io.iohk.ethereum.network.discovery

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.util.ByteString
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.network._
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import java.net.{InetSocketAddress, URI}
import java.time.Clock
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class PeerDiscoveryManager(
    discoveryListener: ActorRef,
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    nodeStatusHolder: AtomicReference[NodeStatus],
    clock: Clock
) extends Actor
    with ActorLogging {

  import PeerDiscoveryManager._

  val expirationTimeSec = discoveryConfig.messageExpiration.toSeconds

  val bootStrapNodesInfo = discoveryConfig.bootstrapNodes.map(DiscoveryNodeInfo.fromNode)

  var pingedNodes: Map[ByteString, PingInfo] = Map.empty

  val startingNodes: Map[ByteString, DiscoveryNodeInfo] = {
    val knownNodesURIs =
      if (discoveryConfig.discoveryEnabled) knownNodesStorage.getKnownNodes()
      else Set.empty
    val startingNodesInfo = knownNodesURIs.map(uri => DiscoveryNodeInfo.fromUri(uri)) ++ bootStrapNodesInfo
    val startingNodesInfoWithoutSelf = startingNodesInfo.filterNot {
      _.node.id == ByteString(nodeStatusHolder.get().nodeId)
    }
    startingNodesInfoWithoutSelf.map { nodeInfo => nodeInfo.node.id -> nodeInfo }.toMap
  }

  var nodesInfo: Map[ByteString, DiscoveryNodeInfo] = startingNodes

  if (discoveryConfig.discoveryEnabled) {
    discoveryListener ! DiscoveryListener.Subscribe
    context.system.scheduler.scheduleWithFixedDelay(
      discoveryConfig.scanInitialDelay,
      discoveryConfig.scanInterval,
      self,
      Scan
    )
  }

  def scan(): Unit = {
    // Ping a random sample from currently pinged nodes without the answer
    new Random().shuffle(pingedNodes.values).take(2 * discoveryConfig.scanMaxNodes).foreach { pingInfo =>
      val node = pingInfo.nodeinfo.node
      sendPing(Endpoint.makeEndpoint(node.udpSocketAddress, node.tcpPort), node.udpSocketAddress, pingInfo.nodeinfo)
    }

    nodesInfo.values.toSeq
      .sortBy(_.addTimestamp)
      .takeRight(discoveryConfig.scanMaxNodes)
      .foreach { nodeInfo =>
        sendPing(
          Endpoint.makeEndpoint(nodeInfo.node.udpSocketAddress, nodeInfo.node.tcpPort),
          nodeInfo.node.udpSocketAddress,
          nodeInfo
        )
      }
  }

  override def receive: Receive = {
    case DiscoveryListener.MessageReceived(ping: Ping, from, packet) =>
      val to = Endpoint.makeEndpoint(from, ping.from.tcpPort)
      sendMessage(Pong(to, packet.mdc, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(pong: Pong, from, _) =>
      pingedNodes.get(pong.token).foreach { newNodeInfo =>
        val nodeInfoUpdatedTime = newNodeInfo.nodeinfo.copy(addTimestamp = clock.millis())
        pingedNodes -= pong.token
        nodesInfo = updateNodes(nodesInfo, nodeInfoUpdatedTime.node.id, nodeInfoUpdatedTime)
        sendMessage(FindNode(ByteString(nodeStatusHolder.get().nodeId), expirationTimestamp), from)
      }

    case DiscoveryListener.MessageReceived(_: FindNode, from, _) =>
      sendMessage(Neighbours(getNeighbours(nodesInfo), expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(neighbours: Neighbours, _, _) =>
      val toPing = neighbours.nodes
        .filterNot(n => nodesInfo.contains(n.nodeId)) // not already on the list

      toPing.foreach { n =>
        Endpoint.toUdpAddress(n.endpoint).foreach { address =>
          val nodeInfo =
            DiscoveryNodeInfo.fromNode(Node(n.nodeId, address.getAddress, n.endpoint.tcpPort, n.endpoint.udpPort))
          sendPing(n.endpoint, address, nodeInfo)
        }
      }

    case GetDiscoveredNodesInfo =>
      sender() ! DiscoveredNodesInfo(nodesInfo.values.toSet)

    case Scan => scan()
  }

  private def sendPing(toEndpoint: Endpoint, toAddr: InetSocketAddress, nodeInfo: DiscoveryNodeInfo): Unit = {
    nodeStatusHolder.get().discoveryStatus match {
      case ServerStatus.Listening(address) =>
        val from = Endpoint.makeEndpoint(address, getTcpPort)
        val ping = Ping(ProtocolVersion, from, toEndpoint, expirationTimestamp)
        val packet = encodePacket(ping, nodeStatusHolder.get().key)
        getPacketData(packet).foreach { key =>
          pingedNodes = updateNodes(pingedNodes, key, PingInfo(nodeInfo, clock.millis()))
        }
        discoveryListener ! DiscoveryListener.SendPacket(Packet(packet), toAddr)
      case _ =>
        log.warning("UDP server not running. Not sending ping message.")
    }
  }

  private def getTcpPort: Int = nodeStatusHolder.get().serverStatus match {
    case ServerStatus.Listening(addr) => addr.getPort
    case _ => 0
  }

  // FIXME come up with more spohisticated approach to keeping both mdc and sha(packet_data), now it is doubled in Map
  // It turns out that geth and parity sent different validation bytestrings in pong response
  // geth uses mdc, but parity uses sha3(packet_data), so  we need to keep track of both things to do not
  // lose large part of potential nodes. https://github.com/ethereumproject/go-ethereum/issues/312
  private def getPacketData(ping: ByteString): List[ByteString] = {
    val packet = Packet(ping)
    val packetMdc = packet.mdc
    val packetDataHash = crypto.kec256(packet.data)
    List(packetMdc, packetDataHash)
  }

  private def updateNodes[V <: TimedInfo](map: Map[ByteString, V], key: ByteString, info: V): Map[ByteString, V] = {
    if (map.size < discoveryConfig.nodesLimit) {
      map + (key -> info)
    } else {
      replaceOldestNode(map, key, info)
    }
  }

  private def replaceOldestNode[V <: TimedInfo](
      map: Map[ByteString, V],
      key: ByteString,
      info: V
  ): Map[ByteString, V] = {
    val (earliestNode, _) = map.minBy { case (_, node) => node.addTimestamp }
    val newMap = map - earliestNode
    newMap + (key -> info)
  }

  private def sendMessage[M <: Message](message: M, to: InetSocketAddress)(implicit rlpEnc: RLPEncoder[M]): Unit = {
    nodeStatusHolder.get().discoveryStatus match {
      case ServerStatus.Listening(_) =>
        val packet = Packet(encodePacket(message, nodeStatusHolder.get().key))
        discoveryListener ! DiscoveryListener.SendPacket(packet, to)
      case _ =>
        log.warning(s"UDP server not running. Not sending message $message.")
    }
  }

  private def getNeighbours(nodesInfo: Map[ByteString, DiscoveryNodeInfo]): Seq[Neighbour] = {
    val randomNodes = new Random().shuffle(nodesInfo.values).take(discoveryConfig.maxNeighbours).toSeq
    randomNodes.map(nodeInfo =>
      Neighbour(Endpoint.makeEndpoint(nodeInfo.node.udpSocketAddress, nodeInfo.node.tcpPort), nodeInfo.node.id)
    )
  }

  private def expirationTimestamp = clock.instant().plusSeconds(expirationTimeSec).getEpochSecond
}

object PeerDiscoveryManager {
  def props(
      discoveryListener: ActorRef,
      discoveryConfig: DiscoveryConfig,
      knownNodesStorage: KnownNodesStorage,
      nodeStatusHolder: AtomicReference[NodeStatus],
      clock: Clock
  ): Props =
    Props(new PeerDiscoveryManager(discoveryListener, discoveryConfig, knownNodesStorage, nodeStatusHolder, clock))

  object DiscoveryNodeInfo {

    def fromUri(uri: URI): DiscoveryNodeInfo = fromNode(Node.fromUri(uri))

    def fromNode(node: Node): DiscoveryNodeInfo = DiscoveryNodeInfo(node, System.currentTimeMillis())

  }

  sealed abstract class TimedInfo {
    def addTimestamp: Long
  }
  case class DiscoveryNodeInfo(node: Node, addTimestamp: Long) extends TimedInfo
  case class PingInfo(nodeinfo: DiscoveryNodeInfo, addTimestamp: Long) extends TimedInfo

  case object GetDiscoveredNodesInfo
  case class DiscoveredNodesInfo(nodes: Set[DiscoveryNodeInfo])

  private[discovery] case object Scan
}
