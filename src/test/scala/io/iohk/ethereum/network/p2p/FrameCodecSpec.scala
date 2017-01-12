package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.rlp
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class FrameCodecSpec extends FlatSpec with Matchers {

  "FrameCodec" should "send and receive Ping message" in new SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)

    val msg: Ping = Ping()
    val data: ByteString = frameCodec.writeFrame(msg.code, ByteString(rlp.encode(msg)))

    val readFrames: Seq[Frame] = remoteFrameCodec.readFrames(data)
    val firstFrame: Frame = readFrames.head
    val readMessage: Message = Message.decode(firstFrame.`type`, firstFrame.payload)

    readMessage shouldBe Ping()
  }

  it should "send and receive Hello message" in new SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)

    val msg: Hello = Hello(1, "test-client", Seq(Capability("foo", 1)), 3000, ByteString("123456"))
    val data: ByteString = frameCodec.writeFrame(msg.code, ByteString(rlp.encode(msg)))

    val readFrames: Seq[Frame] = remoteFrameCodec.readFrames(data)
    val firstFrame: Frame = readFrames.head
    val readMessage: Message = Message.decode(firstFrame.`type`, firstFrame.payload)

    readMessage shouldBe msg
  }

  it should "send message and receive a response" in new SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)

    val ping = Ping()
    val pingData: ByteString = frameCodec.writeFrame(ping.code, ByteString(rlp.encode(ping)))
    val pingReadFrames: Seq[Frame] = remoteFrameCodec.readFrames(pingData)
    val pingReadMessage: Message = Message.decode(pingReadFrames.head.`type`, pingReadFrames.head.payload)

    val pong = Pong()
    val pongData: ByteString = remoteFrameCodec.writeFrame(pong.code, ByteString(rlp.encode(pong)))
    val pongReadFrames: Seq[Frame] = frameCodec.readFrames(pongData)
    val pongReadMessage: Message = Message.decode(pongReadFrames.head.`type`, pongReadFrames.head.payload)

    pingReadMessage shouldBe ping
    pongReadMessage shouldBe pong
  }

}
