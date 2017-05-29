package io.iohk.ethereum.network.rlpx

import java.util.concurrent.atomic.AtomicInteger

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.{Message, MessageDecoder, MessageSerializable}

import scala.util.Try

class MessageCodec(frameCodec: FrameCodec, messageDecoder: MessageDecoder, protocolVersion: Message.Version) {

  val MaxFramePayloadSize: Int = Int.MaxValue // no framing

  val contextIdCounter = new AtomicInteger

  def readMessages(data: ByteString): Seq[Try[Message]] = {
    val frames = frameCodec.readFrames(data)
    frames map { frame => Try(messageDecoder.fromBytes(frame.`type`, frame.payload.toArray, protocolVersion)) }
  }

  def encodeMessage(serializable: MessageSerializable): ByteString = {
    val encoded: Array[Byte] = serializable.toBytes
    val numFrames = Math.ceil(encoded.length / MaxFramePayloadSize.toDouble).toInt
    val contextId = contextIdCounter.incrementAndGet()

    val frames = (0 until numFrames) map { frameNo =>
      val payload = encoded.drop(frameNo * MaxFramePayloadSize).take(MaxFramePayloadSize)
      val totalPacketSize = if (frameNo == 0) Some(encoded.length) else None
      val header =
        if (numFrames > 1) Header(payload.length, 0, Some(contextId), totalPacketSize)
        else Header(payload.length, 0, None, None)
      Frame(header, serializable.code, ByteString(payload))
    }

    frameCodec.writeFrames(frames)
  }

}
