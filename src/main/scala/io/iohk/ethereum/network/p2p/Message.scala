package io.iohk.ethereum.network.p2p

import akka.util.ByteString

import cats.implicits._

import io.iohk.ethereum.utils.Logger

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

@FunctionalInterface
trait MessageDecoder extends Logger { self =>

  type DecodingError = Throwable // TODO: Replace Throwable with an ADT when feasible

  def fromBytes(`type`: Int, payload: Array[Byte]): Either[DecodingError, Message]

  def fromBytesUnsafe(`type`: Int, payload: Array[Byte]): Message = self.fromBytes(`type`, payload) match {
    case Left(err) => throw err
    case Right(res) => res
  }

  def orElse(otherMessageDecoder: MessageDecoder): MessageDecoder = new MessageDecoder {
    override def fromBytes(`type`: Int, payload: Array[Byte]): Either[DecodingError, Message] =
    self.fromBytes(`type`, payload).leftFlatMap { err =>
        log.debug(err.getLocalizedMessage())
        otherMessageDecoder.fromBytes(`type`, payload)
      }
  }
}
