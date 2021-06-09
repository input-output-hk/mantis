package io.iohk.ethereum.network.rlpx

import java.util.concurrent.atomic.AtomicInteger
import akka.util.ByteString
import io.iohk.ethereum.network.handshaker.EtcHelloExchangeState
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.p2p.{Message, MessageDecoder, MessageSerializable}
import org.xerial.snappy.Snappy

import scala.util.{Failure, Success, Try}

object MessageCodec {
  val MaxFramePayloadSize: Int = Int.MaxValue // no framing
  // 16Mb in base 2
  val MaxDecompressedLength = 16777216
}

class MessageCodec(
    frameCodec: FrameCodec,
    messageDecoder: MessageDecoder,
    protocolVersion: Capability,
    val remotePeer2PeerVersion: Long
) {
  import MessageCodec._

  val contextIdCounter = new AtomicInteger

  // TODO: ETCM-402 - messageDecoder should use negotiated protocol version
  def readMessages(data: ByteString): Seq[Try[Message]] = {
    val frames = frameCodec.readFrames(data)
    readFrames(frames)
  }

  def readFrames(frames: Seq[Frame]): Seq[Try[Message]] = {
    frames map { frame =>
      val frameData = frame.payload.toArray
      val payloadTry =
        if (remotePeer2PeerVersion >= EtcHelloExchangeState.P2pVersion && frame.`type` != Hello.code) {
          decompressData(frameData)
        } else {
          Success(frameData)
        }

      payloadTry.map { payload =>
        messageDecoder.fromBytes(frame.`type`, payload, protocolVersion)
      }
    }
  }

  private def decompressData(data: Array[Byte]): Try[Array[Byte]] = {
    Try(Snappy.uncompressedLength(data)).flatMap { decompressedSize =>
      if (decompressedSize > MaxDecompressedLength)
        Failure(new RuntimeException("Message size larger than 16mb"))
      else
        Try(Snappy.uncompress(data))
    }
  }

  def encodeMessage(serializable: MessageSerializable): ByteString = {
    val encoded: Array[Byte] = serializable.toBytes
    val numFrames = Math.ceil(encoded.length / MaxFramePayloadSize.toDouble).toInt
    val contextId = contextIdCounter.incrementAndGet()
    val frames = (0 until numFrames) map { frameNo =>
      val framedPayload = encoded.drop(frameNo * MaxFramePayloadSize).take(MaxFramePayloadSize)
      val payload =
        if (remotePeer2PeerVersion >= EtcHelloExchangeState.P2pVersion && serializable.code != Hello.code) {
          Snappy.compress(framedPayload)
        } else {
          framedPayload
        }

      val totalPacketSize = if (frameNo == 0) Some(encoded.length) else None
      val header =
        if (numFrames > 1) Header(payload.length, 0, Some(contextId), totalPacketSize)
        else Header(payload.length, 0, None, None)
      Frame(header, serializable.code, ByteString(payload))
    }

    frameCodec.writeFrames(frames)
  }

}
