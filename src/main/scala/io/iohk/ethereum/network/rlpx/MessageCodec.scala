package io.iohk.ethereum.network.rlpx

import java.util.concurrent.atomic.AtomicInteger

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPEncoder

import scala.util.Try

class MessageCodec(frameCodec: FrameCodec, protocolVersion: Int) {

  val MaxFramePayloadSize: Int = Int.MaxValue // no framing

  val contextIdCounter = new AtomicInteger

  def readMessages(data: ByteString): Seq[Try[Message]] = {
    val frames = frameCodec.readFrames(data)
    frames map { frame => Try(Message.decode(frame.`type`, frame.payload.toArray, protocolVersion)) }
  }

  def encodeMessage[M <: Message : RLPEncoder](message: M): ByteString = {
    val encoded = rlp.encode(message)
    val numFrames = Math.ceil(encoded.length / MaxFramePayloadSize.toDouble).toInt
    val contextId = contextIdCounter.incrementAndGet()

    val frames = (0 until numFrames) map { frameNo =>
      val payload = encoded.drop(frameNo * MaxFramePayloadSize).take(MaxFramePayloadSize)
      val totalPacketSize = if (frameNo == 0) Some(encoded.length) else None
      val header = Header(payload.length, 0, Some(contextId), totalPacketSize)
      Frame(header, message.code, ByteString(payload))
    }

    frameCodec.writeFrames(frames)
  }

}
