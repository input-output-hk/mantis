package io.iohk.ethereum.network.discovery

import java.net.{InetSocketAddress, URI}

import io.iohk.ethereum.network._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{ByteUtils, NodeStatus, ServerStatus}

import scala.concurrent.ExecutionContext.Implicits.global

class PeerDiscoveryManager(
    discoveryListener: ActorRef,
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    nodeStatusHolder: Agent[NodeStatus]) extends Actor with ActorLogging {

  import PeerDiscoveryManager._

  var nodes: Map[ByteString, NodeWithTimestamp] = {
    val bootStrapNodes = discoveryConfig.bootstrapNodes.map(NodeWithTimestamp.fromNode)
    val knownNodesURIs =
      if (discoveryConfig.discoveryEnabled) knownNodesStorage.getKnownNodes()
      else Set.empty
    val nodes = bootStrapNodes ++ knownNodesURIs.map(NodeWithTimestamp.fromUri)

    nodes.map { node => node.id -> node }.toMap
  }

  if (discoveryConfig.discoveryEnabled) {
    discoveryListener ! DiscoveryListener.Subscribe
    context.system.scheduler.schedule(discoveryConfig.scanInitialDelay, discoveryConfig.scanInterval, self, Scan)
  }

  def scan(): Unit = {
    nodes.values.toSeq
      .sortBy(_.addTimestamp) // take 10 most recent nodes
      .takeRight(discoveryConfig.scanMaxNodes)
      .foreach { node => sendPing(node.id, node.addr) }
  }

  override def receive: Receive = {
    case DiscoveryListener.MessageReceived(ping: Ping, from, packet) =>
      val to = Endpoint(packet.nodeId, from.getPort, from.getPort)
      sendMessage(Pong(to, packet.mdc, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(pong: Pong, from, packet) =>
      val newNode = NodeWithTimestamp.fromNode(NodeImpl(packet.nodeId, from))

      if (nodes.size < discoveryConfig.nodesLimit) {
        nodes += newNode.id -> newNode
        sendMessage(FindNode(ByteString(nodeStatusHolder().nodeId), expirationTimestamp), from)
      } else {
        val (earliestNode, _) = nodes.minBy { case (_, node) => node.addTimestamp }
        nodes -= earliestNode
        nodes += newNode.id -> newNode
      }

    case DiscoveryListener.MessageReceived(findNode: FindNode, from, packet) =>
      sendMessage(Neighbours(Nil, expirationTimestamp), from)

    case DiscoveryListener.MessageReceived(neighbours: Neighbours, from, packet) =>
      val toPing = neighbours.nodes
        .filterNot(n => nodes.contains(n.nodeId)) // not already on the list
        .take(discoveryConfig.nodesLimit - nodes.size)

      toPing.foreach { n =>
        sendPing(n.nodeId, new InetSocketAddress(ByteUtils.bytesToIp(n.endpoint.address), n.endpoint.udpPort))
      }

    case GetDiscoveredNodes =>
      sender() ! DiscoveredNodes(nodes.values.toSet)

    case Scan => scan()
  }

  private def sendPing(toNodeId: ByteString, toAddr: InetSocketAddress): Unit = {
    val tcpPort = nodeStatusHolder().serverStatus match {
      case ServerStatus.Listening(addr) => addr.getPort
      case _ => 0
    }
    nodeStatusHolder().discoveryStatus match {
      case ServerStatus.Listening(address) =>
        val from = Endpoint(ByteString(address.getAddress.getAddress), address.getPort, tcpPort)
        val to = Endpoint(toNodeId, toAddr.getPort, toAddr.getPort)
        sendMessage(Ping(ProtocolVersion, from, to, expirationTimestamp), toAddr)
      case _ =>
        log.warning("UDP server not running. Not sending ping message.")
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

  private def expirationTimestamp = discoveryConfig.messageExpiration.toSeconds + System.currentTimeMillis() / 1000
}

object PeerDiscoveryManager {
  def props(discoveryListener: ActorRef,
            discoveryConfig: DiscoveryConfig,
            knownNodesStorage: KnownNodesStorage,
            nodeStatusHolder: Agent[NodeStatus]): Props =
    Props(new PeerDiscoveryManager(discoveryListener, discoveryConfig, knownNodesStorage, nodeStatusHolder))

  object NodeWithTimestamp {

    def fromUri(uri: URI): NodeWithTimestamp = fromNode(NodeImpl.fromUri(uri))

    def fromNode(node: Node): NodeWithTimestamp = NodeWithTimestamp(node.id, node.addr, System.currentTimeMillis())

  }

  case class NodeWithTimestamp(id: ByteString, addr: InetSocketAddress, addTimestamp: Long) extends Node

  case object GetDiscoveredNodes
  case class DiscoveredNodes(nodes: Set[NodeWithTimestamp])

  private case object Scan
}
