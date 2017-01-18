package io.iohk.ethereum.network.p2p.messages

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect.Reasons
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp._
import org.spongycastle.util.encoders.Hex

object WireProtocol {

  object Capability {
    implicit val rlpEncDec = new RLPEncoder[Capability] with RLPDecoder[Capability] {
      override def encode(obj: Capability): RLPEncodeable = {
        RLPList(obj.name, obj.version)
      }

      override def decode(rlp: RLPEncodeable): Capability = rlp match {
        case RLPList(name, version) => Capability(name, version)
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
        case RLPList(p2pVersion, clientId, (capabilities: RLPList), listenPort, nodeId) =>
          Hello(p2pVersion, clientId, capabilities.items.map(Capability.rlpEncDec.decode),
            listenPort, ByteString(nodeId: Array[Byte]))
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

    override val code: Int = Hello.code

    override def toString: String = {
      s"""Hello {
         |p2pVersion: $p2pVersion
         |clientId: $clientId
         |capabilities: $capabilities
         |listenPort: $listenPort
         |nodeId: ${Hex.toHexString(nodeId.toArray[Byte])}
         |}""".stripMargin
    }
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

    object Reasons {
      val DisconnectRequested = 0x00
      val TcpSubsystemError = 0x01
      val UselessPeer = 0x03
      val TooManyPeers = 0x04
      val AlreadyConnected = 0x05
      val IncompatibleP2pProtocolVersion = 0x06
      val NullNodeIdentityReceived = 0x07
      val ClientQuitting = 0x08
      val UnexpectedIdentity = 0x09
      val IdentityTheSame = 0xa
      val TimeoutOnReceivingAMessage = 0x0b
      val Other = 0x10
    }

    val code = 0x01
  }

  case class Disconnect(reason: Long) extends Message {
    override val code: Int = Disconnect.code

    override def toString: String = {

      val message = reason match {
        case Reasons.DisconnectRequested => "Disconnect requested"
        case Reasons.TcpSubsystemError => "TCP sub-system error"
        case Reasons.UselessPeer => "Useless peer"
        case Reasons.TooManyPeers => "Too many peers"
        case Reasons.AlreadyConnected => "Already connected"
        case Reasons.IncompatibleP2pProtocolVersion => "Incompatible P2P protocol version"
        case Reasons.NullNodeIdentityReceived => "Null node identity received - this is automatically invalid"
        case Reasons.ClientQuitting => "Client quitting"
        case Reasons.UnexpectedIdentity => "Unexpected identity"
        case Reasons.IdentityTheSame => "Identity is the same as this node"
        case Reasons.TimeoutOnReceivingAMessage => "Timeout on receiving a message"
        case Reasons.Other => "Some other reason specific to a subprotocol"
        case other => s"unknown reason code: $other"
      }

      s"Disconnect($message)"
    }
  }

  object Ping {

    implicit val rlpEndDec = new RLPEncoder[Ping] with RLPDecoder[Ping] {
      override def encode(obj: Ping): RLPEncodeable = RLPList()

      override def decode(rlp: RLPEncodeable): Ping = Ping()
    }

    val code = 0x02
  }

  case class Ping() extends Message {
    override val code: Int = Ping.code
  }

  object Pong {

    implicit val rlpEndDec = new RLPEncoder[Pong] with RLPDecoder[Pong] {
      override def encode(obj: Pong): RLPEncodeable = RLPList()

      override def decode(rlp: RLPEncodeable): Pong = Pong()
    }

    val code = 0x03
  }

  case class Pong() extends Message {
    override val code: Int = Pong.code
  }

}
