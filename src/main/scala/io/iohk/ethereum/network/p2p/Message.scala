package io.iohk.ethereum.network.p2p

import akka.util.ByteString

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
  def fromBytes(`type`: Int, payload: Array[Byte]): Message

  def orElse(otherMessageDecoder: MessageDecoder): MessageDecoder = (`type`: Int, payload: Array[Byte]) =>
    Try {
      self.fromBytes(`type`, payload)
    }.getOrElse(otherMessageDecoder.fromBytes(`type`, payload))
}
