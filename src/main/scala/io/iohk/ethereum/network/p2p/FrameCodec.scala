package io.iohk.ethereum.network.p2p

import java.io.IOException

import akka.util.ByteString
import io.iohk.ethereum.network.Secrets
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp._
import io.iohk.ethereum.rlp.RLPImplicits._
import org.spongycastle.crypto.StreamCipher
import org.spongycastle.crypto.digests.KeccakDigest
import org.spongycastle.crypto.engines.AESFastEngine
import org.spongycastle.crypto.modes.SICBlockCipher
import org.spongycastle.crypto.params.{KeyParameter, ParametersWithIV}

case class Frame(header: Header, `type`: Int, payload: Array[Byte])

case class Header(bodySize: Int, protocol: Int, contextId: Option[Int], totalFrameSize: Option[Int])

class FrameCodec(private val secrets: Secrets) {

  private val blockSize = secrets.aes.length * 8

  private val enc: StreamCipher = {
    val cipher = new SICBlockCipher(new AESFastEngine)
    cipher.init(true, new ParametersWithIV(new KeyParameter(secrets.aes), new Array[Byte](blockSize / 8)))
    cipher
  }

  private val dec: StreamCipher = {
    val cipher = new SICBlockCipher(new AESFastEngine)
    cipher.init(false, new ParametersWithIV(new KeyParameter(secrets.aes), new Array[Byte](blockSize / 8)))
    cipher
  }

  private var unprocessedData: ByteString = ByteString.empty

  private var headerOpt: Option[Header] = None

  def readFrames(data: ByteString): Seq[Frame] = {
    unprocessedData ++= data

    def readRecursive(): Seq[Frame] = {
      if (headerOpt.isEmpty) tryReadHeader()

      headerOpt map { header =>
        val padding = (16 - (header.bodySize % 16)) % 16
        val macSize = 16
        val totalSizeToRead = header.bodySize + padding + macSize

        if (unprocessedData.length >= totalSizeToRead) {
          val buffer = unprocessedData.take(totalSizeToRead).toArray

          val frameSize = totalSizeToRead - macSize
          secrets.ingressMac.update(buffer, 0, frameSize)
          dec.processBytes(buffer, 0, frameSize, buffer, 0)

          val `type` = rlp.decode[Int](buffer)

          val pos = rlp.nextElementIndex(buffer, 0)
          val payload = buffer.drop(pos).take(header.bodySize - pos)
          val size = header.bodySize - pos
          val macBuffer = new Array[Byte](secrets.ingressMac.getDigestSize)

          doSum(secrets.ingressMac, macBuffer)
          updateMac(secrets.ingressMac, macBuffer, 0, buffer, frameSize, egress = false)

          headerOpt = None
          unprocessedData = unprocessedData.drop(totalSizeToRead)
          Seq(Frame(header, `type`, payload)) ++ readRecursive()
        } else Nil
      } getOrElse Nil
    }

    readRecursive()
  }

  private def tryReadHeader(): Unit = {
    if (unprocessedData.size >= 32) {
      val headBuffer = unprocessedData.take(32).toArray

      updateMac(secrets.ingressMac, headBuffer, 0, headBuffer, 16, egress = false)

      dec.processBytes(headBuffer, 0, 16, headBuffer, 0)

      var bodySize: Int = headBuffer(0)
      bodySize = (bodySize << 8) + (headBuffer(1) & 0xFF)
      bodySize = (bodySize << 8) + (headBuffer(2) & 0xFF)

      val rlpList = rlp.decode[Seq[Int]](headBuffer.drop(3))(seqEncDec[Int]).lift
      val protocol = rlpList(0).get
      val contextId = rlpList(1)
      val totalFrameSize = rlpList(2)

      unprocessedData = unprocessedData.drop(32)
      headerOpt = Some(Header(bodySize, protocol, contextId, totalFrameSize))
    }
  }

  def writeFrame(`type`: Int, payload: ByteString, contextId: Option[Int] = None, totalFrameSize: Option[Int] = None): ByteString = {
    var out: ByteString = ByteString("")

    val headBuffer = new Array[Byte](32)
    val ptype = rlp.encode(`type`)

    val totalSize: Int = payload.length + ptype.length
    headBuffer(0) = (totalSize >> 16).toByte
    headBuffer(1) = (totalSize >> 8).toByte
    headBuffer(2) = totalSize.toByte

    var headerDataElems: Seq[Array[Byte]] = Nil
    headerDataElems :+= rlp.encode(0)
    contextId.foreach { cid => headerDataElems :+= rlp.encode(cid) }
    totalFrameSize foreach { tfs => headerDataElems :+= rlp.encode(tfs) }

    val headerData = rlp.encode(headerDataElems)(seqEncDec[Array[Byte]])
    System.arraycopy(headerData, 0, headBuffer, 3, headerData.length)

    enc.processBytes(headBuffer, 0, 16, headBuffer, 0)

    updateMac(secrets.egressMac, headBuffer, 0, headBuffer, 16, egress = true)

    val buff: Array[Byte] = new Array[Byte](256)
    out ++= ByteString(headBuffer)

    enc.processBytes(ptype.toArray, 0, ptype.length, buff, 0)
    out ++= ByteString(buff.take(ptype.length))
    secrets.egressMac.update(buff, 0, ptype.length)

    var i = 0
    while (i < payload.length) {
      val bytes = payload.drop(i).take(256).toArray
      enc.processBytes(bytes, 0, bytes.length, bytes, 0)
      secrets.egressMac.update(bytes, 0, bytes.length)
      out ++= bytes
      i += bytes.length
    }
    val padding: Int = 16 - (totalSize % 16)
    val pad: Array[Byte] = new Array[Byte](16)
    if (padding < 16) {
      enc.processBytes(pad, 0, padding, buff, 0)
      secrets.egressMac.update(buff, 0, padding)
      out ++= buff.take(padding)
    }

    val macBuffer: Array[Byte] = new Array[Byte](secrets.egressMac.getDigestSize)
    doSum(secrets.egressMac, macBuffer)
    updateMac(secrets.egressMac, macBuffer, 0, macBuffer, 0, egress = true)
    out ++= macBuffer.take(16)

    out
  }

  private def makeMacCipher: AESFastEngine = {
    val macc = new AESFastEngine
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
