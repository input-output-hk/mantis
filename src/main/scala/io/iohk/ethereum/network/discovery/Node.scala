package io.iohk.ethereum.network.discovery

import java.net.{InetSocketAddress, _}

import akka.util.ByteString
import io.iohk.ethereum.utils.Logger
import org.spongycastle.util.encoders.Hex

import scala.util.{Failure, Success, Try}

case class Node(id: ByteString, addr: InetSocketAddress) {
  def toUri: URI = {
    val host = addr.getHostName
    val port = addr.getPort
    new URI(s"enode://${Hex.toHexString(id.toArray[Byte])}@$host:$port")
  }
}

object Node {
  def fromUri(uri: URI): Node = {
    val nodeId = ByteString(Hex.decode(uri.getUserInfo))
    Node(nodeId, new InetSocketAddress(uri.getHost, uri.getPort))
  }
}

object NodeParser extends Logger {
  val NodeScheme = "enode"
  val NodeIdSize = 64

  /**
    * Parse a node string, for it to be valid it should have the format:
    * "enode://[128 char (64bytes) hex string]@[IPv4 address]:[port]"
    *
    * @param node to be parsed
    * @return the parsed node, or the error detected during parsing
    */
  def parseNode(node: String): Either[Set[Throwable], Node] = {

    val maybeUri = Try(new URI(node))

    val maybeScheme = maybeUri.map(_.getScheme).flatMap { scheme =>
      if (scheme == NodeScheme) Success(scheme)
      else Failure(new Exception(s"Invalid node scheme $scheme, it should be $NodeScheme"))
    }

    val maybeNodeId = maybeUri
      .flatMap(uri => Try(ByteString(Hex.decode(uri.getUserInfo))))
      .flatMap { nodeId =>
        if (nodeId.size == NodeIdSize) Success(nodeId)
        else Failure(new Exception(s"Invalid nodeId size ${nodeId.size}, it should be $NodeIdSize bytes long"))
      }

    val maybeAddress = maybeUri
      .flatMap(uri => Try(InetAddress.getByName(uri.getHost) -> uri.getPort))
      .flatMap{ case (host, port) => host match {
        case _: Inet4Address => Success(host -> port)
        case _ =>
          //FIXME: We currently don't support IPv6 nodes [EC-295]
          Failure(new Exception(s"Invalid host $host, only IPv4 addresses are currently supported"))
      }}
      .flatMap{ case (host, port) => Try(new InetSocketAddress(host, port))}

    (maybeScheme, maybeNodeId, maybeAddress) match {
      case (Success(_), Success(n), Success(addr)) =>
        Right(Node(n, addr))
      case _ =>
        Left(Set(maybeScheme, maybeNodeId, maybeAddress).flatMap(_.toEither.left.toSeq))
    }
  }

  /**
    * Parses a set of nodes, logging the invalid ones and returning the valid ones
    *
    * @param unParsedNodes, nodes to be parsed
    * @return set of parsed and valid nodes
    */
  def parseNodes(unParsedNodes: Set[String]): Set[Node] = unParsedNodes.foldLeft[Set[Node]](Set.empty) {
    case (parsedNodes, nodeString) =>
      val maybeNode = NodeParser.parseNode(nodeString)
      maybeNode match {
        case Right(node) => parsedNodes + node
        case Left(errors) =>
          log.warn(s"Unable to parse node: $nodeString due to: ${errors.map(_.getMessage).mkString("; ")}")
          parsedNodes
      }
  }
}
