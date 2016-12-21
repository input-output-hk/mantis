package io.iohk.ethereum.network

import java.io.IOException
import java.net.{URI, Socket}

import akka.util.ByteString
import io.iohk.ethereum.crypto._

object TestSocketHandshaker {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val remoteUri = new URI(args(0))

    val (initiatePacket, authHandshaker) = AuthHandshaker(nodeKey).initiate(remoteUri)

    val sock = new Socket(remoteUri.getHost, remoteUri.getPort)
    val inp = sock.getInputStream
    val out = sock.getOutputStream

    println("Sending initiate packet")
    out.write(initiatePacket.toArray)

    val responsePacket = new Array[Byte](AuthResponseMessage.encodedLength + ECIESCoder.getOverhead)
    val n: Int = inp.read(responsePacket)
    if (n < responsePacket.length) throw new IOException("could not read, got " + n)
    println("Received response packet")

    val result = authHandshaker.handleResponseMessage(ByteString(responsePacket))
    println("Auth handshake result: " + result)
  }

}
