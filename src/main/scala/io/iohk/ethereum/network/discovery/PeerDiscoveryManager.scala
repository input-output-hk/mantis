package io.iohk.ethereum.network.discovery

import java.net.{InetSocketAddress, URI}
import java.time.Clock

import io.iohk.ethereum.network._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}

import scala.concurrent.ExecutionContext.Implicits.global

class PeerDiscoveryManager(
    discoveryListener: ActorRef,
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    nodeStatusHolder: Agent[NodeStatus],
    clock: Clock) extends Actor with ActorLogging {

  import PeerDiscoveryManager._

  val expirationTimeSec = discoveryConfig.messageExpiration.toSeconds

  val bootStrapNodesInfo = discoveryConfig.bootstrapNodes.map(DiscoveryNodeInfo.fromNode)

  var pingedNodes: Map[ByteString, DiscoveryNodeInfo] = Map.empty

  var nodesInfo: Map[ByteString, DiscoveryNodeInfo] = {
    val knownNodesURIs =
      if (discoveryConfig.discoveryEnabled) knownNodesStorage.getKnownNodes()
      else Set.empty
    val nodesInfo = knownNodesURIs.map(uri => DiscoveryNodeInfo.fromUri(uri))
    nodesInfo.map { nodeInfo => nodeInfo.node.id -> nodeInfo }.toMap
  }

  if (discoveryConfig.discoveryEnabled) {
    discoveryListener ! DiscoveryListener.Subscribe
    context.system.scheduler.schedule(discoveryConfig.scanInitialDelay, discoveryConfig.scanInterval, self, Scan)
  }

  def scan(): Unit = {
    // Always ping bootstrap nodes as they have many neighbours and we do not want lose any info about them
    // One optimisation worth considering is pinging them only every n-th scan
    bootStrapNodesInfo.foreach{ nodeInfo =>
      sendPing(Endpoint.makeEndpoint(nodeInfo.node.udpSocketAddress, nodeInfo.node.tcpPort), nodeInfo.node.udpSocketAddress, nodeInfo)
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
      pingedNodes.get(packet.nodeId).foreach { newNodeInfo =>
        pingedNodes -= newNodeInfo.node.id
        if (nodesInfo.size < discoveryConfig.nodesLimit) {
          nodesInfo += newNodeInfo.node.id -> newNodeInfo
          sendMessage(FindNode(ByteString(nodeStatusHolder().nodeId), expirationTimestamp), from)
        } else {
          val (earliestNode, _) = nodesInfo.minBy { case (_, node) => node.addTimestamp }
          nodesInfo -= earliestNode
          nodesInfo += newNodeInfo.node.id -> newNodeInfo
        }
      }

    case DiscoveryListener.MessageReceived(findNode: FindNode, from, packet) =>
      sendMessage(Neighbours(Nil, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(neighbours: Neighbours, from, packet) =>
      val toPing = neighbours.nodes
        .filterNot(n => nodesInfo.contains(n.nodeId)) // not already on the list
        .take(discoveryConfig.nodesLimit - nodesInfo.size)

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
    val tcpPort = nodeStatusHolder().serverStatus match {
      case ServerStatus.Listening(addr) => addr.getPort
      case _ => 0
    }
    nodeStatusHolder().discoveryStatus match {
      case ServerStatus.Listening(address) =>
        val from = Endpoint.makeEndpoint(address, tcpPort)
        updatePingedNodes(nodeInfo)
        sendMessage(Ping(ProtocolVersion, from, toEndpoint, expirationTimestamp), toAddr)
      case _ =>
        log.warning("UDP server not running. Not sending ping message.")
    }
  }

  private def updatePingedNodes(nodeInfo: DiscoveryNodeInfo): Unit = {
    if (pingedNodes.size < discoveryConfig.nodesLimit) {
      pingedNodes += nodeInfo.node.id -> nodeInfo
    } else {
      val (earliestNode, _) = nodesInfo.minBy { case (_, node) => node.addTimestamp }
      nodesInfo -= earliestNode
      nodesInfo += nodeInfo.node.id -> nodeInfo
    }
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

  case class DiscoveryNodeInfo(node: Node, addTimestamp: Long)

  case object GetDiscoveredNodesInfo
  case class DiscoveredNodesInfo(nodes: Set[DiscoveryNodeInfo])

  private[discovery] case object Scan
}
