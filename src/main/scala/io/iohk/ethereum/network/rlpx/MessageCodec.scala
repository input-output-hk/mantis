package io.iohk.ethereum.network.rlpx

import java.util.concurrent.atomic.AtomicInteger

import akka.util.ByteString
import io.iohk.ethereum.network.handshaker.EtcHelloExchangeState
import io.iohk.ethereum.network.p2p.messages.WireProtocol.Hello
import io.iohk.ethereum.network.p2p.{Message, MessageDecoder, MessageSerializable}
import org.xerial.snappy.Snappy

import scala.util.{Failure, Success, Try}

class MessageCodec(frameCodec: FrameCodec, messageDecoder: MessageDecoder, protocolVersion: Message.Version) {

  val MaxFramePayloadSize: Int = Int.MaxValue // no framing

  val contextIdCounter = new AtomicInteger

  // 16Mb in base 2
  val maxDecompressedLength = 16777216

  // MessageCodec is only used from actor context so it can be var
  @volatile
  private var remotePeerP2pVersion: Option[Long] = None

  private def setRemoteVersionBasedOnHelloMessage(m: Message): Unit = {
    if (remotePeerP2pVersion.isEmpty) {
      m match {
        case hello: Hello =>
          remotePeerP2pVersion = Some(hello.p2pVersion)
        case _ =>
      }
    }
  }

  // TODO: ETCM-402 - messageDecoder should use negotiated protocol version
  def readMessages(data: ByteString): Seq[Try[Message]] = {
    val frames = frameCodec.readFrames(data)

    frames.map { frame =>
      val frameData = frame.payload.toArray
      val payloadTry =
        if (remotePeerP2pVersion.exists(version => version >= EtcHelloExchangeState.P2pVersion)) {
          decompressData(frameData)
        } else {
          Success(frameData)
        }

      payloadTry.map { payload =>
        val m = messageDecoder.fromBytes(frame.`type`, payload, protocolVersion)
        setRemoteVersionBasedOnHelloMessage(m)
        m
      }
    }
  }

  private def decompressData(data: Array[Byte]): Try[Array[Byte]] = {
    Try(Snappy.uncompressedLength(data)).flatMap { decompressedSize =>
      if (decompressedSize > maxDecompressedLength)
        Failure(new RuntimeException("Message size larger than 16mb"))
      else
        Try(Snappy.uncompress(data))
    }
  }

  def encodeMessage(serializable: MessageSerializable): ByteString = {
    val encoded: Array[Byte] = serializable.toBytes
    val numFrames = Math.ceil(encoded.length / MaxFramePayloadSize.toDouble).toInt
    val contextId = contextIdCounter.incrementAndGet()
    val frames = (0 until numFrames).map { frameNo =>
      val framedPayload = encoded.drop(frameNo * MaxFramePayloadSize).take(MaxFramePayloadSize)
      val payload =
        if (
          remotePeerP2pVersion
            .exists(version => version >= EtcHelloExchangeState.P2pVersion) && serializable.code != Hello.code
        ) {
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
