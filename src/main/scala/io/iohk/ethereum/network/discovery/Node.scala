package io.iohk.ethereum.network.discovery

import java.net._

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex

import scala.util.Try

case class Node(id: ByteString, address: InetSocketAddress) {
  def toUri: URI = {
    val host = address.getHostName
    val port = address.getPort
    new URI(s"enode://${Hex.toHexString(id.toArray[Byte])}@$host:$port")
  }
}

object Node {
  def fromUri(uri: URI): Node = {
    val nodeId = ByteString(Hex.decode(uri.getUserInfo))
    Node(nodeId, new InetSocketAddress(uri.getHost, uri.getPort))
  }
}

object NodeParser {
  val NodeScheme = "enode"
  val NodeIdSize = 64

  /**
    * Parse a node string, for it to be valid it should have the format:
    * "enode://[128 char (64bytes) hex string]@[IPv4 address]:[port]"
    *
    * @param node to be parsed
    * @return the parsed node, or the error detected during parsing
    */
  def parseNode(node: String): Try[Node] = Try {
    val maybeUri = Try(new URI(node))

    require(maybeUri.isSuccess, s"Invalid format of node: '$node'")
    val uri = maybeUri.get

    val scheme = uri.getScheme
    val maybeNodeId = Try(ByteString(Hex.decode(uri.getUserInfo)))
    val maybeHost = Try(InetAddress.getByName(uri.getHost))
    val port = uri.getPort

    require(maybeNodeId.isSuccess, s"Invalid nodeId '${Option(uri.getUserInfo).getOrElse("")}'")
    require(maybeHost.isSuccess, s"Invalid host '${Option(uri.getHost).getOrElse("")}'")
    val nodeId = maybeNodeId.get
    val host = maybeHost.get

    require(scheme == NodeScheme, s"Invalid node scheme $scheme, only $NodeScheme is supported")
    require(nodeId.size == NodeIdSize, s"Invalid nodeId size ${nodeId.size}, it should be $NodeIdSize bytes long")

    val address = new InetSocketAddress(host, port)
    //FIXME: We currently don't support IPv6 nodes
    require(host.isInstanceOf[Inet4Address], s"Invalid host $host, only IPv4 addresses are currently supported")

    Node(nodeId, address)
  }
}
