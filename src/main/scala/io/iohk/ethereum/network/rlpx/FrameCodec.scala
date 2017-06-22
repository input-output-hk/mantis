package io.iohk.ethereum.network.rlpx

import java.io.IOException

import akka.util.ByteString
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.crypto.StreamCipher
import org.spongycastle.crypto.digests.KeccakDigest
import org.spongycastle.crypto.engines.AESEngine
import org.spongycastle.crypto.modes.SICBlockCipher
import org.spongycastle.crypto.params.{KeyParameter, ParametersWithIV}

import scala.annotation.tailrec

case class Frame(header: Header, `type`: Int, payload: ByteString)

case class Header(bodySize: Int, protocol: Int, contextId: Option[Int], totalPacketSize: Option[Int])

class FrameCodec(private val secrets: Secrets) {

  val HeaderLength = 32
  val MacSize = 16

  private val allZerosIV = Array.fill[Byte](16)(0)

  private val enc: StreamCipher = {
    val cipher = new SICBlockCipher(new AESEngine)
    cipher.init(true, new ParametersWithIV(new KeyParameter(secrets.aes), allZerosIV))
    cipher
  }

  private val dec: StreamCipher = {
    val cipher = new SICBlockCipher(new AESEngine)
    cipher.init(false, new ParametersWithIV(new KeyParameter(secrets.aes), allZerosIV))
    cipher
  }

  private var unprocessedData: ByteString = ByteString.empty

  private var headerOpt: Option[Header] = None

  /**
    * Note, this method is not reentrant.
    *
    * @param data
    * @return
    */
  def readFrames(data: ByteString): Seq[Frame] = {
    unprocessedData ++= data

    @tailrec
    def readRecursive(framesSoFar: Seq[Frame] = Nil): Seq[Frame] = {
      if (headerOpt.isEmpty) tryReadHeader()

      headerOpt match {
        case Some(header) =>
          val padding = (16 - (header.bodySize % 16)) % 16
          val totalSizeToRead = header.bodySize + padding + MacSize

          if (unprocessedData.length >= totalSizeToRead) {
            val buffer = unprocessedData.take(totalSizeToRead).toArray

            val frameSize = totalSizeToRead - MacSize
            secrets.ingressMac.update(buffer, 0, frameSize)
            dec.processBytes(buffer, 0, frameSize, buffer, 0)

            val `type` = rlp.decode[Int](buffer)

            val pos = rlp.nextElementIndex(buffer, 0)
            val payload = buffer.slice(pos, header.bodySize)
            val macBuffer = new Array[Byte](secrets.ingressMac.getDigestSize)

            doSum(secrets.ingressMac, macBuffer)
            updateMac(secrets.ingressMac, macBuffer, 0, buffer, frameSize, egress = false)

            headerOpt = None
            unprocessedData = unprocessedData.drop(totalSizeToRead)
            readRecursive(framesSoFar ++ Seq(Frame(header, `type`, ByteString(payload))))
          } else framesSoFar

        case None => framesSoFar
      }
    }

    readRecursive()
  }

  private def tryReadHeader(): Unit = {
    if (unprocessedData.size >= HeaderLength) {
      val headBuffer = unprocessedData.take(HeaderLength).toArray

      updateMac(secrets.ingressMac, headBuffer, 0, headBuffer, 16, egress = false)

      dec.processBytes(headBuffer, 0, 16, headBuffer, 0)

      var bodySize: Int = headBuffer(0)
      bodySize = (bodySize << 8) + (headBuffer(1) & 0xFF)
      bodySize = (bodySize << 8) + (headBuffer(2) & 0xFF)

      val rlpList = rlp.decode[Seq[Int]](headBuffer.drop(3))(seqEncDec[Int]).lift
      val protocol = rlpList(0).get
      val contextId = rlpList(1)
      val totalPacketSize = rlpList(2)

      unprocessedData = unprocessedData.drop(HeaderLength)
      headerOpt = Some(Header(bodySize, protocol, contextId, totalPacketSize))
    }
  }

  def writeFrames(frames: Seq[Frame]): ByteString = {
    val bytes = frames.zipWithIndex flatMap { case (frame, index) =>
      val firstFrame = index == 0
      val lastFrame = index == frames.size - 1

      var out: ByteString = ByteString()

      val headBuffer = new Array[Byte](HeaderLength)
      val ptype = rlp.encode(frame.`type`)

      val totalSize =
        if (firstFrame) frame.payload.length + ptype.length
        else frame.payload.length

      headBuffer(0) = (totalSize >> 16).toByte
      headBuffer(1) = (totalSize >> 8).toByte
      headBuffer(2) = totalSize.toByte

      var headerDataElems: Seq[Array[Byte]] = Nil
      headerDataElems :+= rlp.encode(frame.header.protocol)
      frame.header.contextId.foreach { cid => headerDataElems :+= rlp.encode(cid) }
      frame.header.totalPacketSize foreach { tfs => headerDataElems :+= rlp.encode(tfs) }

      val headerData = rlp.encode(headerDataElems)(seqEncDec[Array[Byte]])
      System.arraycopy(headerData, 0, headBuffer, 3, headerData.length)
      enc.processBytes(headBuffer, 0, 16, headBuffer, 0)
      updateMac(secrets.egressMac, headBuffer, 0, headBuffer, 16, egress = true)

      val buff: Array[Byte] = new Array[Byte](256)
      out ++= ByteString(headBuffer)

      if (firstFrame) {
        // packet-type only in first frame
        enc.processBytes(ptype.toArray, 0, ptype.length, buff, 0)
        out ++= ByteString(buff.take(ptype.length))
        secrets.egressMac.update(buff, 0, ptype.length)
      }

      out ++= processFramePayload(frame.payload)

      if (lastFrame) {
        // padding and mac only in last frame
        out ++= processFramePadding(totalSize)
        out ++= processFrameMac()
      }

      out
    }

    ByteString(bytes.toArray)
  }

  private def processFramePayload(payload: ByteString): ByteString = {
    var i = 0
    var out = ByteString()
    while (i < payload.length) {
      val bytes = payload.drop(i).take(256).toArray
      enc.processBytes(bytes, 0, bytes.length, bytes, 0)
      secrets.egressMac.update(bytes, 0, bytes.length)
      out ++= bytes
      i += bytes.length
    }
    out
  }

  private def processFramePadding(totalSize: Int): ByteString = {
    val padding = 16 - (totalSize % 16)
    if (padding < 16) {
      val pad = new Array[Byte](16)
      val buff = new Array[Byte](16)
      enc.processBytes(pad, 0, padding, buff, 0)
      secrets.egressMac.update(buff, 0, padding)
      ByteString(buff.take(padding))
    } else ByteString()
  }

  private def processFrameMac(): ByteString = {
    val macBuffer = new Array[Byte](secrets.egressMac.getDigestSize)
    doSum(secrets.egressMac, macBuffer)
    updateMac(secrets.egressMac, macBuffer, 0, macBuffer, 0, egress = true)
    ByteString(macBuffer.take(16))
  }

  private def makeMacCipher: AESEngine = {
    val macc = new AESEngine
    macc.init(true, new KeyParameter(secrets.mac.toArray))
    macc
  }

  private def updateMac(mac: KeccakDigest, seed: Array[Byte], offset: Int, out: Array[Byte], outOffset: Int, egress: Boolean): Array[Byte] = {
    val aesBlock = new Array[Byte](mac.getDigestSize)
    doSum(mac, aesBlock)
    makeMacCipher.processBlock(aesBlock, 0, aesBlock, 0)

    val length = 16

    (0 until length) foreach { i =>
      aesBlock(i) = (aesBlock(i) ^ seed(i + offset)).toByte
    }

    mac.update(aesBlock, 0, length)
    val result = new Array[Byte](mac.getDigestSize)
    doSum(mac, result)

    if (egress) System.arraycopy(result, 0, out, outOffset, length)
    else (0 until length) foreach { i =>
      if (out(i + outOffset) != result(i)) throw new IOException("MAC mismatch")
    }

    result
  }

  private def doSum(mac: KeccakDigest, out: Array[Byte]) = {
    new KeccakDigest(mac).doFinal(out, 0)
  }

}
