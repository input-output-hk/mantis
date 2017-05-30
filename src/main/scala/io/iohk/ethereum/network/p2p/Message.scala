package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message.Version

import scala.util.Try

object Message {
  type Version = Int
}

trait Message {
  def code: Int
}

trait MessageSerializable extends Message {

  //DummyImplicit parameter only used to differentiate from the other toBytes method
  def toBytes(implicit di: DummyImplicit): ByteString

  def toBytes: Array[Byte]

}

trait MessageDecoder { self =>
  def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Message.Version): Try[Message]

  def orElse(messageDecoder: MessageDecoder): MessageDecoder = new MessageDecoder {
    override def fromBytes(`type`: Int, payload: Array[Byte], protocolVersion: Version): Try[Message] =
      self.fromBytes(`type`, payload, protocolVersion).orElse(
        messageDecoder.fromBytes(`type`, payload, protocolVersion))
  }
}
