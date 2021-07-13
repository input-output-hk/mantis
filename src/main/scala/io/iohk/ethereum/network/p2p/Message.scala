package io.iohk.ethereum.network.p2p

import cats.implicits._

import io.iohk.ethereum.utils.Logger

trait Message {
  def code: Int
  def toShortString: String
}

trait MessageSerializable extends Message {
  def toBytes: Array[Byte]
  def underlyingMsg: Message
}

@FunctionalInterface
trait MessageDecoder extends Logger { self =>
  import MessageDecoder._

  def fromBytes(`type`: Int, payload: Array[Byte]): Either[DecodingError, Message]

  def orElse(otherMessageDecoder: MessageDecoder): MessageDecoder = new MessageDecoder {
    override def fromBytes(`type`: Int, payload: Array[Byte]): Either[DecodingError, Message] =
      self.fromBytes(`type`, payload).leftFlatMap { err =>
        log.debug(err.getLocalizedMessage())
        otherMessageDecoder.fromBytes(`type`, payload)
      }
  }
}

object MessageDecoder {
  type DecodingError = Throwable // TODO: Replace Throwable with an ADT when feasible
}
