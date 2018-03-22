package io.iohk.ethereum.network.discovery

import java.net.{InetSocketAddress, URI}
import java.time.Clock

import io.iohk.ethereum.network._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.network.discovery.DiscoveryListener.Packet
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class PeerDiscoveryManager(
    discoveryListener: ActorRef,
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    nodeStatusHolder: Agent[NodeStatus],
    clock: Clock) extends Actor with ActorLogging {

  import PeerDiscoveryManager._

  val expirationTimeSec = discoveryConfig.messageExpiration.toSeconds

  val bootStrapNodesInfo = discoveryConfig.bootstrapNodes.map(DiscoveryNodeInfo.fromNode)

  var pingedNodes: Map[ByteString, PingInfo] = Map.empty

  var nodesInfo: Map[ByteString, DiscoveryNodeInfo] = {
    val knownNodesURIs =
      if (discoveryConfig.discoveryEnabled) knownNodesStorage.getKnownNodes()
      else Set.empty
    val nodesInfo = knownNodesURIs.map(uri => DiscoveryNodeInfo.fromUri(uri)) ++ bootStrapNodesInfo
    nodesInfo.map { nodeInfo => nodeInfo.node.id -> nodeInfo }.toMap
  }

  if (discoveryConfig.discoveryEnabled) {
    discoveryListener ! DiscoveryListener.Subscribe
    context.system.scheduler.schedule(discoveryConfig.scanInitialDelay, discoveryConfig.scanInterval, self, Scan)
  }

  def scan(): Unit = {
    // Ping a random sample from currently pinged nodes without the answer
    new Random().shuffle(pingedNodes.values).take(2 * discoveryConfig.scanMaxNodes).foreach {pingInfo =>
      val node = pingInfo.nodeinfo.node
      sendPing(Endpoint.makeEndpoint(node.udpSocketAddress, node.tcpPort), node.udpSocketAddress, pingInfo.nodeinfo)
    }

    nodesInfo.values.toSeq
      .sortBy(_.addTimestamp)
      .takeRight(discoveryConfig.scanMaxNodes)
      .foreach { nodeInfo =>
        sendPing(Endpoint.makeEndpoint(nodeInfo.node.udpSocketAddress, nodeInfo.node.tcpPort), nodeInfo.node.udpSocketAddress, nodeInfo)
      }
  }

  override def receive: Receive = {
    case DiscoveryListener.MessageReceived(ping: Ping, from, packet) =>
      val to = Endpoint.makeEndpoint(from, ping.from.tcpPort)
      sendMessage(Pong(to, packet.mdc, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(pong: Pong, from, packet) =>
      pingedNodes.get(pong.token).foreach { newNodeInfo =>
        val nodeInfoUpdatedTime = newNodeInfo.nodeinfo.copy(addTimestamp = clock.millis())
        pingedNodes -= pong.token
        nodesInfo = updateNodes(nodesInfo, nodeInfoUpdatedTime.node.id, nodeInfoUpdatedTime)
        sendMessage(FindNode(ByteString(nodeStatusHolder().nodeId), expirationTimestamp), from)
      }

    case DiscoveryListener.MessageReceived(findNode: FindNode, from, packet) =>
      sendMessage(Neighbours(Nil, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(neighbours: Neighbours, from, packet) =>
      val toPing = neighbours.nodes
        .filterNot(n => nodesInfo.contains(n.nodeId)) // not already on the list

      toPing.foreach { n =>
        Endpoint.toUdpAddress(n.endpoint).foreach { address =>
          val nodeInfo = DiscoveryNodeInfo.fromNode(Node(n.nodeId, address.getAddress, n.endpoint.tcpPort, n.endpoint.udpPort))
          sendPing(n.endpoint, address, nodeInfo)
        }
      }

    case GetDiscoveredNodesInfo =>
      sender() ! DiscoveredNodesInfo(nodesInfo.values.toSet)

    case Scan => scan()
  }

  private def sendPing(toEndpoint: Endpoint, toAddr: InetSocketAddress, nodeInfo: DiscoveryNodeInfo): Unit = {
    nodeStatusHolder().discoveryStatus match {
      case ServerStatus.Listening(address) =>
        val from = Endpoint.makeEndpoint(address, getTcpPort)
        val ping = Ping(ProtocolVersion, from, toEndpoint, expirationTimestamp)
        pingedNodes = updateNodes(pingedNodes, getPingMdc(ping), PingInfo(nodeInfo, clock.millis()))
        sendMessage(ping, toAddr)
      case _ =>
        log.warning("UDP server not running. Not sending ping message.")
    }
  }

  private def getTcpPort: Int = nodeStatusHolder().serverStatus match {
    case ServerStatus.Listening(addr) => addr.getPort
    case _ => 0
  }

  private def getPingMdc(ping: Ping): ByteString =
    Packet(DiscoveryListener.encodePacket(ping, nodeStatusHolder().key)).mdc

  private def updateNodes[V <: TimedInfo](map: Map[ByteString, V], key: ByteString, info: V): Map[ByteString, V] = {
    if (map.size < discoveryConfig.nodesLimit) {
      map + (key -> info)
    } else {
      replaceOldestNode(map, key, info)
    }
  }

  private def replaceOldestNode[V <: TimedInfo](map: Map[ByteString, V], key: ByteString, info: V): Map[ByteString, V] = {
    val (earliestNode, _) = map.minBy { case (_, node) => node.addTimestamp }
    val newMap = map - earliestNode
    newMap + (key -> info)
  }

  private def sendMessage[M <: Message](message: M, to: InetSocketAddress)(implicit rlpEnc: RLPEncoder[M]): Unit = {
    nodeStatusHolder().discoveryStatus match {
      case ServerStatus.Listening(_) =>
        discoveryListener ! DiscoveryListener.SendMessage(message, to)
      case _ =>
        log.warning(s"UDP server not running. Not sending message $message.")
    }
  }

  private def expirationTimestamp = clock.instant().plusSeconds(expirationTimeSec).getEpochSecond
}

object PeerDiscoveryManager {
  def props(discoveryListener: ActorRef,
            discoveryConfig: DiscoveryConfig,
            knownNodesStorage: KnownNodesStorage,
            nodeStatusHolder: Agent[NodeStatus],
            clock: Clock): Props =
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
