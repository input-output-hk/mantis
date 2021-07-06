package io.iohk.ethereum.network.p2p

import akka.util.ByteString

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.iohk.ethereum.network.rlpx.Frame
import io.iohk.ethereum.network.rlpx.FrameCodec
import io.iohk.ethereum.network.rlpx.Header
import io.iohk.ethereum.rlp.RLPEncodeable
import io.iohk.ethereum.rlp.RLPImplicitConversions._
import io.iohk.ethereum.rlp.RLPImplicits._
import io.iohk.ethereum.rlp.RLPList
import io.iohk.ethereum.rlp.RLPSerializable
import io.iohk.ethereum.rlp.rawDecode

class FrameCodecSpec extends AnyFlatSpec with Matchers {

  import DummyMsg._

  it should "send message and receive a response" in new SecureChannelSetup {
    val frameCodec = new FrameCodec(secrets)
    val remoteFrameCodec = new FrameCodec(remoteSecrets)

    val sampleMessage = DummyMsg(2310, ByteString("Sample Message"))
    val sampleMessageEncoded: ByteString = sampleMessage.toBytes
    val sampleMessageFrame = Frame(
      Header(sampleMessageEncoded.length, 0, None, Some(sampleMessageEncoded.length)),
      sampleMessage.code,
      sampleMessageEncoded
    )
    val sampleMessageData = remoteFrameCodec.writeFrames(Seq(sampleMessageFrame))
    val sampleMessageReadFrames = frameCodec.readFrames(sampleMessageData)
    val sampleMessageReadMessage = sampleMessageReadFrames.head.payload.toArray[Byte].toSample

    sampleMessageReadMessage shouldBe sampleMessage
  }

  object DummyMsg {
    val code: Int = 2323

    implicit class DummyMsgEnc(val underlyingMsg: DummyMsg) extends MessageSerializable with RLPSerializable {
      override def code: Int = DummyMsg.code

      override def toRLPEncodable: RLPEncodeable = RLPList(underlyingMsg.aField, underlyingMsg.anotherField)
      override def toShortString: String = underlyingMsg.toShortString
    }

    implicit class DummyMsgDec(val bytes: Array[Byte]) {
      def toSample: DummyMsg = rawDecode(bytes) match {
        case RLPList(aField, anotherField) => DummyMsg(aField, anotherField)
        case _                             => throw new RuntimeException("Cannot decode Status")
      }
    }
  }

  case class DummyMsg(aField: Int, anotherField: ByteString) extends Message {
    override def code: Int = DummyMsg.code
    override def toShortString: String = toString
  }

}
