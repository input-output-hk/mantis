package io.iohk.ethereum.network.rlpx

import akka.util.ByteString
import io.iohk.ethereum.rlp
import io.iohk.ethereum.rlp.RLPImplicits._

import scala.annotation.tailrec

class VmFrameCodec extends FrameCodec {

  val HeaderLength = 16

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
          val totalSizeToRead = header.bodySize // + padding // + MacSize

          if (unprocessedData.length >= totalSizeToRead) {
            val buffer = unprocessedData.take(totalSizeToRead).toArray

            val `type` = rlp.decode[Int](buffer)

            val pos = rlp.nextElementIndex(buffer, 0)
            val payload = buffer.slice(pos, header.bodySize)

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

      out ++= ByteString(headBuffer)

      if (firstFrame) {
        out ++= ByteString(ptype)
      }

      out ++= processFramePayload(frame.payload)
      out
    }

    ByteString(bytes.toArray)
  }

  private def processFramePayload(payload: ByteString): ByteString = {
    var i = 0
    var out = ByteString()
    while (i < payload.length) {
      val bytes = payload.drop(i).take(256).toArray
      out ++= bytes
      i += bytes.length
    }
    out
  }

}
