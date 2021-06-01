package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.messages.Capability

import scala.util.Try

trait Message {
  def code: Int
  def toShortString: String
}

trait MessageSerializable extends Message {

  //DummyImplicit parameter only used to differentiate from the other toBytes method
  def toBytes(implicit di: DummyImplicit): ByteString

  def toBytes: Array[Byte]

  def underlyingMsg: Message
}

trait MessageDecoder { self =>
  def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Capability): Message

  def orElse(otherMessageDecoder: MessageDecoder): MessageDecoder = new MessageDecoder {
    override def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Capability): Message =
      Try { self.fromBytes(`type`, payload, protocolVersion) }
        .getOrElse(otherMessageDecoder.fromBytes(`type`, payload, protocolVersion))
  }
}
