package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.utils._
import io.iohk.ethereum.utils.RLPImplicits._

object Message {

  def decode(`type`: Int, payload: Array[Byte]): Option[Message] = {
    `type` match {
      case Hello.code => Some(RLP.decode[Hello](payload)(Hello.rlpEndDec).get) // TODO implicit encDec (sbt)
      case Ping.code => Some(RLP.decode[Ping](payload)(Ping.rlpEndDec).get)
      case Pong.code => Some(RLP.decode[Pong](payload)(Pong.rlpEndDec).get)
      case _ => None
    }
  }

}

sealed trait Message {
  def code: Int
}

object Capability {
  implicit val rlpEncDec = new RLPEncoder[Capability] with RLPDecoder[Capability] {
    override def encode(obj: Capability): RLPEncodeable = {
      RLPList(obj.name, obj.version)
    }

    override def decode(rlp: RLPEncodeable): Capability = rlp match {
      case rlpList: RLPList => Capability(rlpList.items(0), rlpList.items(1))
      case _ => throw new RuntimeException("Cannot decode Capability")
    }
  }
}

case class Capability(name: String, version: Byte)

object Hello {

  implicit val rlpEndDec = new RLPEncoder[Hello] with RLPDecoder[Hello] {
    override def encode(obj: Hello): RLPEncodeable = {
      import obj._
      RLPList(p2pVersion, clientId, capabilities, listenPort, nodeId)
    }

    override def decode(rlp: RLPEncodeable): Hello = rlp match {
      case rlpList: RLPList =>
        Hello(p2pVersion = rlpList.items(0),
          clientId = rlpList.items(1),
          capabilities = rlpList.items(2).asInstanceOf[RLPList].items.map(Capability.rlpEncDec.decode),
          listenPort = rlpList.items(3),
          nodeId = ByteString(rlpList.items(4): Array[Byte]))
      case _ => throw new RuntimeException("Cannot decode Hello")
    }
  }

  val code = 0x80

}

case class Hello(
    p2pVersion: Int,
    clientId: String,
    capabilities: Seq[Capability],
    listenPort: Int,
    nodeId: ByteString)
  extends Message {

  override val code = Hello.code
}

object Endpoint {
  implicit val rlpEncDec = new RLPEncoder[Endpoint] with RLPDecoder[Endpoint] {
    override def encode(obj: Endpoint): RLPEncodeable = {
      RLPList(obj.address, obj.udpPort, obj.tcpPort)
    }

    override def decode(rlp: RLPEncodeable): Endpoint = rlp match {
      case rlpList: RLPList => Endpoint(ByteString(rlpList.items(0): Array[Byte]), rlpList.items(1), rlpList.items(2))
      case _ => throw new RuntimeException("Cannot decode Endpoint")
    }
  }
}

case class Endpoint(
      address: ByteString,
      udpPort: Int,
      tcpPort: Int)

object Ping {

  implicit val rlpEndDec = new RLPEncoder[Ping] with RLPDecoder[Ping] {
    override def encode(obj: Ping): RLPEncodeable = {
      RLPList(obj.version, obj.from, obj.to, obj.timestamp)
    }

    override def decode(rlp: RLPEncodeable): Ping = rlp match {
      case rlpList: RLPList =>
        Ping(version = rlpList.items(0),
          from = Endpoint.rlpEncDec.decode(rlpList.items(1)),
          to = Endpoint.rlpEncDec.decode(rlpList.items(2)), // TODO: implicit?
          timestamp = rlpList.items(3))
      case _ => throw new RuntimeException("Cannot decode Ping")
    }
  }

  val code = 0x01
}

case class Ping(
    version: Int,
    from: Endpoint,
    to: Endpoint,
    timestamp: Long)
  extends Message {

  override val code = Ping.code
}

object Pong {

  implicit val rlpEndDec = new RLPEncoder[Pong] with RLPDecoder[Pong] {
    override def encode(obj: Pong): RLPEncodeable = {
      RLPList(obj.to, obj.echo, obj.timestamp)
    }

    override def decode(rlp: RLPEncodeable): Pong = rlp match {
      case rlpList: RLPList =>
        Pong(
          to = Endpoint.rlpEncDec.decode(rlpList.items(0)),
          echo = rlpList.items(1),
          timestamp = rlpList.items(2))
      case _ => throw new RuntimeException("Cannot decode Pong")
    }
  }

  val code = 0x02
}

case class Pong(
    to: Endpoint,
    echo: Array[Byte],
    timestamp: Long)
  extends Message {

  override val code = Pong.code
}


