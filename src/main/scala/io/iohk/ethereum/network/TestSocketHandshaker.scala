package io.iohk.ethereum.network

import java.io.{InputStream, OutputStream}
import java.net.{Socket, URI}

import org.spongycastle.crypto.params.ECPublicKeyParameters

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}
import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.p2p._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.Status
import io.iohk.ethereum.network.p2p.messages.PV62.{BlockBodies, BlockHeaders, GetBlockBodies, GetBlockHeaders}
import io.iohk.ethereum.network.p2p.Message.PV63
import io.iohk.ethereum.network.p2p.messages.PV63.{GetNodeData, GetReceipts, NodeData, Receipts}
import io.iohk.ethereum.network.p2p.messages.WireProtocol._
import io.iohk.ethereum.rlp.{encode => rlpEncode}
import io.iohk.ethereum.rlp._
import org.spongycastle.crypto.AsymmetricCipherKeyPair

object TestSocketHandshaker {

  val nodeKey: AsymmetricCipherKeyPair = generateKeyPair()
  val nodeId: Array[Byte] = nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId

  def main(args: Array[String]): Unit = {
    val P2PVersion: Int = 4
    val ListenPort: Int = 3333

    val remoteUri = new URI(args(0))

    val (initiatePacket, authHandshaker) = AuthHandshaker(nodeKey).initiate(remoteUri)

    val sock = new Socket(remoteUri.getHost, remoteUri.getPort)
    val inp = sock.getInputStream
    val out = sock.getOutputStream

    out.write(initiatePacket.toArray)

    val responsePacket = new Array[Byte](AuthResponseMessage.encodedLength + ECIESCoder.getOverhead)
    inp.read(responsePacket)

    val result = authHandshaker.handleResponseMessage(ByteString(responsePacket))
    val secrets = result.asInstanceOf[AuthHandshakeSuccess].secrets
    val frameCodec = new FrameCodec(secrets)

    val helloMsg = Hello(
      p2pVersion = P2PVersion,
      clientId = "etc-client",
      capabilities = Seq(Capability("eth", PV63.toByte)),
      listenPort = ListenPort,
      nodeId = ByteString(nodeId))

    sendMessage(helloMsg, frameCodec, out)
    val remoteHello = readAtLeastOneMessage(frameCodec, inp).head.asInstanceOf[Hello]

    while (true) {
      val msgs = readAtLeastOneMessage(frameCodec, inp)
      msgs.foreach { m =>
        //println("\n Received message: " + m)
      }

      handleMessage(msgs,frameCodec, out)
    }
  }

  private def handleMessage(msgs: Seq[Message], frameCodec: FrameCodec, out: OutputStream) = {
    val MaxHeaders = 1
    val BlockNumber = 1382228
    val firstBlockAfterDaoFork = 1920000

    msgs.collect {
      case _: Ping => sendMessage(Pong(), frameCodec, out)
      case m: GetBlockHeaders if m.block.fold(a => a.equals(BigInt(firstBlockAfterDaoFork)), _ => false) =>
        sendMessage(BlockHeaders(Seq.empty), frameCodec, out)
      case m: Status =>
        sendMessage(m.copy(bestHash = m.genesisHash), frameCodec, out) //send same status message
        //ask for block further in chain to get some transactions
        sendMessage(GetBlockHeaders(Left(BlockNumber), maxHeaders = MaxHeaders, skip = 0, reverse = 0), frameCodec, out)
      case m: BlockHeaders =>
        println(m)
        sendMessage(GetBlockBodies(m.headers.map(h => h.hash)), frameCodec, out) //ask for block bodies for headers
        sendMessage(GetReceipts(m.headers.map(h => h.hash)), frameCodec, out) //ask for recipts
        sendMessage(GetNodeData(Seq(m.headers.map(h => h.hash).head)), frameCodec, out) //ask for node
      case m: BlockBodies =>
        println(m)
      case m: Receipts =>
        println(m)
      case m: NodeData =>
        println(m)
      case m: Disconnect =>
        println(m)
    }
  }

  def sendMessage[M <: Message : RLPEncoder](message: M, frameCodec: FrameCodec, out: OutputStream): Unit = {
    val encoded = rlpEncode(message)
    val frame = frameCodec.writeFrame(message.code, ByteString(encoded))
    println(s"\n Sending message: $message")
    out.write(frame.toArray)
  }

  @tailrec
  def readAtLeastOneMessage(frameCodec: FrameCodec, inp: InputStream): Seq[Message] = {
    val buff = new Array[Byte](1)
    val n = inp.read(buff)
    if (n > 0) {
      val frames = frameCodec.readFrames(ByteString(buff))
      val decodedFrames = frames.map(f => Try(Message.decode(f.`type`, f.payload, PV63)))
      val messages = decodedFrames.collect { case Success(msg) => msg }

      decodedFrames
        .collect { case Failure(ex) => ex }
        .foreach { ex => println(s"Unable to decode frame: $ex") }

      if (messages.nonEmpty) messages
      else readAtLeastOneMessage(frameCodec, inp)

    } else readAtLeastOneMessage(frameCodec, inp)
  }

}
