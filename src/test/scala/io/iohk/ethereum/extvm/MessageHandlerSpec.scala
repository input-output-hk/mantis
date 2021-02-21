package io.iohk.ethereum.extvm

import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Keep, Sink, SinkQueueWithCancel, Source, SourceQueueWithComplete}
import akka.testkit.TestProbe
import akka.util.ByteString
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import io.iohk.ethereum.vm.Generators
import java.math.BigInteger

import com.google.protobuf.CodedOutputStream
import org.bouncycastle.util.BigIntegers
import org.scalamock.scalatest.MockFactory
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalapb.descriptors.{FieldDescriptor, PValue}

class MessageHandlerSpec extends AnyFlatSpec with Matchers with MockFactory with ScalaCheckPropertyChecks {

  import Implicits._
  import akka.pattern.pipe
  import scala.concurrent.duration._

  "MessageHandler" should "send arbitrary messages" in {
    implicit val system = ActorSystem("MessageHandlerSpec_System")

    val bytesGen = Generators.getByteStringGen(1, 1024 * 128)

    forAll(bytesGen) { bytes =>
      val probe = TestProbe()

      val in = mock[SinkQueueWithCancel[ByteString]]
      val (out, fut) = Source.queue[ByteString](1024, OverflowStrategy.dropTail).toMat(Sink.seq)(Keep.both).run()
      fut.pipeTo(probe.ref)

      // mock of final fields is no longer possible
      // had to create a GeneratedMessage stub
      val gm = new GeneratedMessage {
        override def writeTo(output: CodedOutputStream): Unit = output.writeRawBytes(bytes)
        override def getFieldByNumber(fieldNumber: Int): Any = ???
        override def getField(field: FieldDescriptor): PValue = ???
        override def companion: GeneratedMessageCompanion[_] = ???
        override def serializedSize: Int = bytes.size
        override def toProtoString: String = ???
      }
      val messageHandler = new MessageHandler(in, out)
      messageHandler.sendMessage(gm)
      messageHandler.close()

      val lengthBytes = ByteString(BigIntegers.asUnsignedByteArray(LengthPrefixSize, BigInteger.valueOf(bytes.length)))
      probe.expectMsg(3.seconds, Seq(lengthBytes ++ bytes))
    }
  }

  it should "receive arbitrary code messages" in {
    implicit val system = ActorSystem("MessageHandlerSpec_System")

    val bytesGen = Generators.getByteStringGen(1, 8)

    forAll(bytesGen) { bytes =>
      val out = mock[SourceQueueWithComplete[ByteString]]
      val codeMsg = msg.Code(bytes).toByteArray
      val in = Source.single(ByteString(codeMsg)).toMat(Sink.queue())(Keep.right).run()

      val messageHandler = new MessageHandler(in, out)
      val receivedMsg = messageHandler.awaitMessage[msg.Code]
      (receivedMsg.code: ByteString) shouldBe bytes
      messageHandler.close()
    }
  }

}
