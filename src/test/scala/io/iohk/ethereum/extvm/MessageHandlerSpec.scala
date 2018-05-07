package io.iohk.ethereum.extvm

import java.math.BigInteger

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Keep, Sink, SinkQueueWithCancel, Source, SourceQueueWithComplete}
import akka.util.ByteString
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.testkit.TestProbe
import com.trueaccord.scalapb.GeneratedMessage
import io.iohk.ethereum.vm.Generators
import org.scalatest.prop.PropertyChecks
import org.spongycastle.util.BigIntegers

import scala.concurrent.ExecutionContext.Implicits.global

class MessageHandlerSpec extends FlatSpec with Matchers with MockFactory with PropertyChecks {

  import akka.pattern.pipe
  import scala.concurrent.duration._
  import Implicits._

  "MessageHandler" should "send arbitrary messages" in {
    implicit val system = ActorSystem("MessageHandlerSpec_System")
    implicit val materializer = ActorMaterializer()

    val bytesGen = Generators.getByteStringGen(1, 1024 * 128)

    forAll(bytesGen) { bytes =>
      val probe = TestProbe()

      val in = mock[SinkQueueWithCancel[ByteString]]
      val (out, fut) = Source.queue[ByteString](1024, OverflowStrategy.dropTail).toMat(Sink.seq)(Keep.both).run()
      fut.pipeTo(probe.ref)

      val gm = mock[GeneratedMessage]
      (gm.toByteArray _).expects().returning(bytes.toArray[Byte])

      val messageHandler = new MessageHandler(in, out)
      messageHandler.sendMessage(gm)
      messageHandler.close()

      val lengthBytes = ByteString(BigIntegers.asUnsignedByteArray(LengthPrefixSize, BigInteger.valueOf(bytes.length)))
      probe.expectMsg(3.seconds, Seq(lengthBytes ++ bytes))
    }
  }

  it should "receive arbitrary code messages" in {
    implicit val system = ActorSystem("MessageHandlerSpec_System")
    implicit val materializer = ActorMaterializer()

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
