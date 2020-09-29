package io.iohk.ethereum.extvm

import java.math.BigInteger

import akka.stream.scaladsl.{SinkQueueWithCancel, SourceQueueWithComplete}
import akka.util.ByteString
import com.google.protobuf.CodedInputStream
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, LiteParser, Message}
import org.bouncycastle.util.BigIntegers

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class MessageHandler(in: SinkQueueWithCancel[ByteString], out: SourceQueueWithComplete[ByteString]) {

  private val AwaitTimeout = 5.minutes

  def sendMessage[M <: GeneratedMessage](msg: M): Unit = {
    val bytes = msg.toByteArray
    val lengthBytes = ByteString(BigIntegers.asUnsignedByteArray(LengthPrefixSize, BigInteger.valueOf(bytes.length)))

    out offer (lengthBytes ++ ByteString(bytes))
  }

  def awaitMessage[M <: GeneratedMessage with Message[M]](implicit companion: GeneratedMessageCompanion[M]): M = {
    val resF = in.pull() map {
      case Some(bytes) =>
        LiteParser
          .parseDelimitedFrom(CodedInputStream.newInstance(bytes.toArray[Byte]))
          .getOrElse( throw new RuntimeException(s"Unable to parse message: $bytes") )
      case None => throw new RuntimeException("Stream completed")
    }

    Await.result(resF, AwaitTimeout)
  }

  def close(): Unit = {
    Try(in.cancel())
    Try(out.complete())
  }

}
