package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._

object Message {

  def decode(`type`: Int, payload: Array[Byte]): Message = `type` match {
    case Hello.code => rlp.decode(payload)(Hello.rlpEndDec)
    case Disconnect.code => rlp.decode(payload)(Disconnect.rlpEndDec)
    case Ping.code => rlp.decode(payload)(Ping.rlpEndDec)
    case Pong.code => rlp.decode(payload)(Pong.rlpEndDec)
    case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
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
      RLPList(p2pVersion, clientId, capabilities, listenPort, nodeId.toArray[Byte])
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

  val code = 0x00

}

case class Hello(
    p2pVersion: Long,
    clientId: String,
    capabilities: Seq[Capability],
    listenPort: Long,
    nodeId: ByteString)
  extends Message {

  override val code = Hello.code
}

object Ping {

  implicit val rlpEndDec = new RLPEncoder[Ping] with RLPDecoder[Ping] {
    override def encode(obj: Ping): RLPEncodeable = RLPList()
    override def decode(rlp: RLPEncodeable): Ping = Ping()
  }

  val code = 0x02
}

case class Ping() extends Message {
  override val code = Ping.code
}

object Pong {

  implicit val rlpEndDec = new RLPEncoder[Pong] with RLPDecoder[Pong] {
    override def encode(obj: Pong): RLPEncodeable = RLPList()
    override def decode(rlp: RLPEncodeable): Pong = Pong()
  }

  val code = 0x03
}

case class Pong() extends Message {
  override val code = Pong.code
}

object Disconnect {
  implicit val rlpEndDec = new RLPEncoder[Disconnect] with RLPDecoder[Disconnect] {
    override def encode(obj: Disconnect): RLPEncodeable = {
      RLPList(obj.reason)
    }

    override def decode(rlp: RLPEncodeable): Disconnect = rlp match {
      case rlpList: RLPList =>
        Disconnect(reason = rlpList.items.head)
      case _ => throw new RuntimeException("Cannot decode Disconnect")
    }
  }

  val code = 0x01
}

case class Disconnect(reason: Long) extends Message {
  override val code = Disconnect.code
}
