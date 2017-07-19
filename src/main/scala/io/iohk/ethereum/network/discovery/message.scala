package io.iohk.ethereum.network.discovery

import akka.util.ByteString
import io.iohk.ethereum.rlp.{RLPDecoder, RLPEncodeable, RLPEncoder, RLPList}
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPImplicitConversions._

import scala.util.Try

sealed trait Message {
  def packetType: Byte
}

object Endpoint {

  implicit val rlpEncDec = new RLPEncoder[Endpoint] with RLPDecoder[Endpoint] {
    override def encode(obj: Endpoint): RLPEncodeable = {
      import obj._
      RLPList(address, udpPort, tcpPort)
    }

    override def decode(rlp: RLPEncodeable): Endpoint = rlp match {
      case RLPList(address, udpPort, tcpPort, _*) =>
        Endpoint(address, udpPort, tcpPort)
      case _ => throw new RuntimeException("Cannot decode Endpoint")
    }
  }

}

case class Endpoint(address: ByteString, udpPort: Int, tcpPort: Int)

object Ping {
  val packetType: Byte = 0x01

  implicit val rlpEncDec = new RLPEncoder[Ping] with RLPDecoder[Ping] {
    override def encode(obj: Ping): RLPEncodeable = {
      import obj._
      RLPList(version, from, to, timestamp)
    }

    override def decode(rlp: RLPEncodeable): Ping = rlp match {
      case RLPList(version, from, to, timestamp, _*) =>
        Ping(version, Endpoint.rlpEncDec.decode(from), Endpoint.rlpEncDec.decode(to), timestamp)
      case _ => throw new RuntimeException("Cannot decode Ping")
    }
  }

}

case class Ping(version: Int, from: Endpoint, to: Endpoint, timestamp: Long) extends Message {
  override val packetType: Byte = Ping.packetType
}

object Neighbours {
  val packetType: Byte = 0x04

  implicit val rlpEncDec = new RLPEncoder[Neighbours] with RLPDecoder[Neighbours] {
    override def encode(obj: Neighbours): RLPEncodeable = {
      import obj._
      RLPList(RLPList(nodes.map(Neighbour.rlpEncDec.encode): _*), expires)
    }

    override def decode(rlp: RLPEncodeable): Neighbours = rlp match {
      case RLPList(nodes: RLPList, expires, _*) =>
        Neighbours(nodes.items.map(Neighbour.rlpEncDec.decode), expires)
      case _ => throw new RuntimeException("Cannot decode Neighbours")
    }
  }
}

case class Neighbours(nodes: Seq[Neighbour], expires: Long) extends Message {
  override val packetType: Byte = Neighbours.packetType
}

object Neighbour {
  implicit val rlpEncDec = new RLPEncoder[Neighbour] with RLPDecoder[Neighbour] {
    override def encode(obj: Neighbour): RLPEncodeable = {
      import obj._
      RLPList(endpoint, nodeId)
    }

    override def decode(rlp: RLPEncodeable): Neighbour = rlp match {
      case RLPList(address, udpPort, tcpPort, nodeId, _*) =>
        Neighbour(Endpoint(address, udpPort, tcpPort), nodeId)
      case _ => throw new RuntimeException("Cannot decode Neighbour")
    }
  }
}

case class Neighbour(endpoint: Endpoint, nodeId: ByteString)

case class FindNode(target: ByteString, expires: Long) extends Message {
  override val packetType: Byte = FindNode.packetType
}

object FindNode {
  val packetType: Byte = 0x03

  implicit val rlpEncDec = new RLPEncoder[FindNode] with RLPDecoder[FindNode] {
    override def encode(obj: FindNode): RLPEncodeable = {
      import obj._
      RLPList(target, expires)
    }

    override def decode(rlp: RLPEncodeable): FindNode = rlp match {
      case RLPList(target, expires, _*) =>
        FindNode(target, expires)
      case _ => throw new RuntimeException("Cannot decode FindNode")
    }
  }
}

object Pong {
  val packetType: Byte = 0x02

  implicit val rlpEncDec = new RLPEncoder[Pong] with RLPDecoder[Pong] {
    override def encode(obj: Pong): RLPEncodeable = {
      import obj._
      RLPList(to, token, timestamp)
    }

    override def decode(rlp: RLPEncodeable): Pong = rlp match {
      case RLPList(to, token, timestamp, _*) =>
        Pong(Endpoint.rlpEncDec.decode(to), token, timestamp)
      case _ => throw new RuntimeException("Cannot decode Pong")
    }
  }
}

case class Pong(to: Endpoint, token: ByteString, timestamp: Long) extends Message {
  override val packetType: Byte = Pong.packetType
}
