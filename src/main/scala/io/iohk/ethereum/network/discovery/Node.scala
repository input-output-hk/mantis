package io.iohk.ethereum.network.discovery

import java.net._

import akka.util.ByteString
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

trait Node {
  val id: ByteString
  val addr: InetSocketAddress

  def toUri: URI = {
    val host = addr.getHostName
    val port = addr.getPort
    new URI(s"enode://${Hex.toHexString(id.toArray[Byte])}@$host:$port")
  }
}

case class NodeImpl(id: ByteString, addr: InetSocketAddress) extends Node

object NodeImpl {
  def fromUri(uri: URI): NodeImpl = {
    val nodeId = ByteString(Hex.decode(uri.getUserInfo))
    NodeImpl(nodeId, new InetSocketAddress(uri.getHost, uri.getPort))
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
  def parseNode(node: String): Try[Node] = {
    val parseResult = for {
      uri <- Try(new URI(node))
      scheme = uri.getScheme
      nodeId <- Try(ByteString(Hex.decode(uri.getUserInfo)))
      host <- Try(InetAddress.getByName(uri.getHost))
      port = uri.getPort
      address <- Try(new InetSocketAddress(host, port))
    } yield (scheme, nodeId, address)
    parseResult.flatMap{ case (scheme, nodeId, address) =>
      val host = address.getAddress
      val hasIPV4Version = host match {
        case _: Inet4Address => true
        case _ => false
      }

      if(scheme != NodeScheme)
        Failure(throw new Exception(s"Invalid node scheme $scheme, it should be $NodeScheme"))
      else if(nodeId.size != NodeIdSize)
        Failure(throw new Exception(s"Invalid nodeId size ${nodeId.size}, it should be $NodeIdSize bytes long"))
      else if(!hasIPV4Version) //FIXME: We currently don't support IPv6 nodes [EC-295]
        Failure(throw new Exception(s"Invalid host $host, only IPv4 addresses are currently supported"))
      else
        Success(NodeImpl(nodeId, address))
    }
  }
}
