package io.iohk.ethereum.network

import java.io.InputStream
import java.net.{URI, Socket}

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.utils.RLP
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex
import scorex.core.network.AuthHandshakeSuccess

import scala.annotation.tailrec

object TestSocketHandshaker {

  val nodeKey = generateKeyPair()
  val nodeId = nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].getQ.getEncoded(false)

  def main(args: Array[String]): Unit = {
    val msg = Hello(
      3,
      "computronium1",
      Seq(
        Capability("eth", 60.toByte),
      Capability("shh", 2.toByte)
    ),
    3333,
      ByteString(Hex.decode("04E5B3A94853763C573CF772BA19B95C0216B8958A6BE424534B46F93798BA043DBE2B085A33CFBA4B1607C17AE18AB4679F2E885829CEE6A4D6868F80A06420DF")))

    val encoded = RLP.encode(msg).get

    println("enc=" + Hex.toHexString(encoded))

      sys.exit(0)



    val remoteUri = new URI(args(0))

    val (initiatePacket, authHandshaker) = AuthHandshaker(nodeKey).initiate(remoteUri)

    val sock = new Socket(remoteUri.getHost, remoteUri.getPort)
    val inp = sock.getInputStream
    val out = sock.getOutputStream

    println("Sending auth handshake initiate packet")
    out.write(initiatePacket.toArray)

    val responsePacket = new Array[Byte](AuthResponseMessage.encodedLength + ECIESCoder.getOverhead)
    inp.read(responsePacket)
    println("Received auth handshake response packet")

    val result = authHandshaker.handleResponseMessage(ByteString(responsePacket))
    val secrets = result.asInstanceOf[AuthHandshakeSuccess].secrets.asInstanceOf[Secrets]
    println(s"Auth handshake result: $result")

    val frameCodec = new FrameCodec(secrets)

    val helloMsg = Hello(
      p2pVersion = 4,
      clientId = "etc-client",
      capabilities = Seq(Capability("eth", 63.toByte), Capability("eth", 62.toByte)),
      listenPort = 3333,
      nodeId = ByteString(nodeId))

    val helloEncoded = RLP.encode(helloMsg).get
    val helloFrameData = frameCodec.writeFrame(helloMsg.code, None, None, ByteString(helloEncoded))

    println("Sending Hello message")
    out.write(helloFrameData.toArray)

    println("Waiting for Hello response")

    val remoteHello = readAtLeastOneMessage(frameCodec, inp).head.asInstanceOf[Hello]
    println(s"Received Hello: $remoteHello")

    /*
    val pingMsg = Ping(0x03, Endpoint(ByteString(""), 0, 0), Endpoint(ByteString(""), 0, 0), System.currentTimeMillis)
    val pingEncoded = RLP.encode(pingMsg).get
    val pingFrameData = frameCodec.writeFrame(pingMsg.code, None, None, ByteString(pingEncoded))

    println("Sending Ping message")
    out.write(pingFrameData.toArray)

    println("Waiting for Pong response")

    val remotePong = readAtLeastOneMessage(frameCodec, inp).head.asInstanceOf[Pong]

    println(s"Received Pong: $remotePong")
    */
  }

  @tailrec
  def readAtLeastOneMessage(frameCodec: FrameCodec, inp: InputStream): Seq[Message] = {
    val buff = new Array[Byte](1)
    val n = inp.read(buff)
    if (n > 0) {
      val frames = frameCodec.readFrames(ByteString(buff))
      if (frames.nonEmpty) {
        frames map { frame =>
          println("Received a frame, decoding message")
          Message.decode(frame.`type`, frame.payload).get
        }
      } else readAtLeastOneMessage(frameCodec, inp)
    } else readAtLeastOneMessage(frameCodec, inp)
  }

}
