package io.iohk.ethereum.network.discovery

import java.net.{InetSocketAddress, URI}

import io.iohk.ethereum.network._
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.util.ByteString
import io.iohk.ethereum.db.storage.KnownNodesStorage
import io.iohk.ethereum.rlp.RLPEncoder
import io.iohk.ethereum.utils.{ByteUtils, NodeStatus, ServerStatus}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global

class PeerDiscoveryManager(
    discoveryListener: ActorRef,
    discoveryConfig: DiscoveryConfig,
    knownNodesStorage: KnownNodesStorage,
    nodeStatusHolder: Agent[NodeStatus]) extends Actor with ActorLogging {

  import PeerDiscoveryManager._

  var nodes: Map[ByteString, Node] = {
    val uris = discoveryConfig.bootstrapNodes.map(new URI(_)) ++
      (if (discoveryConfig.discoveryEnabled) knownNodesStorage.getKnownNodes()
      else Set.empty)

    uris.map { uri =>
      val node = Node.fromUri(uri)
      node.id -> node
    }.toMap
  }

  if (discoveryConfig.discoveryEnabled) {
    discoveryListener ! DiscoveryListener.Subscribe

    context.system.scheduler.schedule(discoveryConfig.scanInitialDelay, discoveryConfig.scanInterval) {
      scan()
    }
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
      val newNode = Node(packet.nodeId, from, System.currentTimeMillis())

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

  object Node {

    def fromUri(uri: URI): Node = {
      val nodeId = ByteString(Hex.decode(uri.getUserInfo))
      Node(nodeId, new InetSocketAddress(uri.getHost, uri.getPort), System.currentTimeMillis())
    }
  }

  case class Node(id: ByteString, addr: InetSocketAddress, addTimestamp: Long) {
    def toUri: URI = {
      new URI(s"enode://${Hex.toHexString(id.toArray[Byte])}@${addr.getAddress.getHostAddress}:${addr.getPort}")
    }
  }

  case object GetDiscoveredNodes
  case class DiscoveredNodes(nodes: Set[Node])
}
