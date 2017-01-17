package io.iohk.ethereum.network.p2p

import akka.util.ByteString
import io.iohk.ethereum.network.p2p.Message.PV63
import io.iohk.ethereum.network.p2p.messages.WireProtocol.{Capability, Hello, Ping, Pong}
import io.iohk.ethereum.rlp
import org.scalatest.{FlatSpec, Matchers}

class FrameCodecSpec extends FlatSpec with Matchers {

  "FrameCodec" should "send and receive Ping message" in new SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)

    val msg = Ping()
    val encoded = ByteString(rlp.encode(msg))
    val frame = Frame(Header(encoded.length, 0, None, Some(encoded.length)), msg.code, encoded)
    val data = frameCodec.writeFrames(Seq(frame))

    val readFrames = remoteFrameCodec.readFrames(data)
    val firstFrame = readFrames.head
    val readMessage = Message.decode(firstFrame.`type`, firstFrame.payload.toArray, PV63)

    readMessage shouldBe Ping()
  }

  it should "send and receive Hello message" in new SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)

    val msg = Hello(1, "test-client", Seq(Capability("foo", 1)), 3000, ByteString("123456"))
    val encoded = ByteString(rlp.encode(msg))
    val frame = Frame(Header(encoded.length, 0, None, Some(encoded.length)), msg.code, encoded)
    val data = frameCodec.writeFrames(Seq(frame))

    val readFrames = remoteFrameCodec.readFrames(data)
    val firstFrame = readFrames.head
    val readMessage = Message.decode(firstFrame.`type`, firstFrame.payload.toArray, PV63)

    readMessage shouldBe msg
  }

  it should "send message and receive a response" in new SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)

    val ping = Ping()
    val pingEncoded = ByteString(rlp.encode(ping))
    val pingFrame = Frame(Header(pingEncoded.length, 0, None, Some(pingEncoded.length)), ping.code, pingEncoded)
    val pingData = frameCodec.writeFrames(Seq(pingFrame))
    val pingReadFrames = remoteFrameCodec.readFrames(pingData)
    val pingReadMessage = Message.decode(pingReadFrames.head.`type`, pingReadFrames.head.payload.toArray, PV63)

    val pong = Pong()
    val pongEncoded = ByteString(rlp.encode(pong))
    val pongFrame = Frame(Header(pongEncoded.length, 0, None, Some(pongEncoded.length)), pong.code, pongEncoded)
    val pongData = remoteFrameCodec.writeFrames(Seq(pongFrame))
    val pongReadFrames = frameCodec.readFrames(pongData)
    val pongReadMessage = Message.decode(pongReadFrames.head.`type`, pongReadFrames.head.payload.toArray, PV63)

    pingReadMessage shouldBe ping
    pongReadMessage shouldBe pong
  }

}
