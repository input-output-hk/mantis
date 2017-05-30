package io.iohk.ethereum.network.p2p

import io.iohk.ethereum.network.p2p.Message.Version
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Disconnect, Ping, Pong}
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Disconnect._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Ping._
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Pong._

import scala.util.Try

package object messages {
  object Versions {
    val PV61: Version = 61
    val PV62: Version = 62
    val PV63: Version = 63

    val SubProtocolOffset = 0x10
  }

  object PeerMessagesDecoder extends MessageDecoder {

    override def fromBytes(`type`: Version, payload: Array[Byte], protocolVersion: Version): Try[Message] = Try {
      `type` match {
        case Disconnect.code => payload.toDisconnect
        case Ping.code => payload.toPing
        case Pong.code => payload.toPong
        case _ => throw new RuntimeException(s"Unknown message type: ${`type`}")
      }
    }

  }
}
