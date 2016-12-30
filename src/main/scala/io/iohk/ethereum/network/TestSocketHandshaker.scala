package io.iohk.ethereum.network

import java.io.InputStream
import java.net.{URI, Socket}

import scala.annotation.tailrec
import scala.util.{Failure, Success}

import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.utils.RLP
import scorex.core.network.AuthHandshakeSuccess

object TestSocketHandshaker {

  val nodeKey = generateKeyPair()
  val nodeId = nodeIdFromPublicKey(nodeKey.getPublic)

  def main(args: Array[String]): Unit = {
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
      capabilities = Seq(Capability("eth", 62.toByte)),
      listenPort = 3333,
      nodeId = ByteString(nodeId))

    val helloEncoded = RLP.encode(helloMsg).get
    val helloFrameData = frameCodec.writeFrame(helloMsg.code, None, None, ByteString(helloEncoded))

    println("Sending Hello message")
    out.write(helloFrameData.toArray)

    println("Waiting for Hello")

    val remoteHello = readAtLeastOneMessage(frameCodec, inp).head.asInstanceOf[Hello]
    println(s"Received Hello: $remoteHello")

    val pingMsg = Ping()
    val pingEncoded = RLP.encode(pingMsg).get
    val pingFrameData = frameCodec.writeFrame(pingMsg.code, None, None, ByteString(pingEncoded))

    println("Sending Ping message")
    out.write(pingFrameData.toArray)


    while (true) {
      val msgs = readAtLeastOneMessage(frameCodec, inp)
      msgs.foreach { m => println("Received message: " + m) }
    }
  }

  @tailrec
  def readAtLeastOneMessage(frameCodec: FrameCodec, inp: InputStream): Seq[Message] = {
    val buff = new Array[Byte](1)
    val n = inp.read(buff)
    if (n > 0) {
      val frames = frameCodec.readFrames(ByteString(buff))
      val decodedFrames = frames.map(f => Message.decode(f.`type`, f.payload))
      val messages = decodedFrames.collect { case Success(msg) => msg }

      decodedFrames
        .collect { case Failure(ex) => ex }
        .foreach { ex => println(s"Unable to decode frame: $ex") }

      if (messages.nonEmpty) messages
      else readAtLeastOneMessage(frameCodec, inp)

    } else readAtLeastOneMessage(frameCodec, inp)
  }

}
