package io.iohk.ethereum.network.rlpx

import java.util.concurrent.atomic.AtomicInteger

import akka.util.ByteString
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPEncoder

import scala.util.Try

trait MessageDecoder {
  type Version = Int
  def decode(`type`: Int, payload: Array[Byte]): Message
}


trait Message {
  def code: Int
}

class MessageCodec(frameCodec: FrameCodec,
                   decoder: MessageDecoder) {

  val MaxFramePayloadSize: Int = Int.MaxValue // no framing

  val contextIdCounter = new AtomicInteger

  def readMessages(data: ByteString): Seq[Try[Message]] = {
    val frames = frameCodec.readFrames(data)
    frames map { frame => Try(decoder.decode(frame.`type`, frame.payload.toArray)) }
  }

  def encodeMessage[M <: Message : RLPEncoder](message: M): ByteString = {

    val encoded = rlp.encode(message)
    val numFrames = Math.ceil(encoded.length / MaxFramePayloadSize.toDouble).toInt
    val contextId = contextIdCounter.incrementAndGet()

    val frames = (0 until numFrames) map { frameNo =>
      val payload = encoded.drop(frameNo * MaxFramePayloadSize).take(MaxFramePayloadSize)
      val totalPacketSize = if (frameNo == 0) Some(encoded.length) else None
      val header =
        if (numFrames > 1) Header(payload.length, 0, Some(contextId), totalPacketSize)
        else Header(payload.length, 0, None, None)
      Frame(header, message.code, ByteString(payload))
    }

    frameCodec.writeFrames(frames)
  }

}
