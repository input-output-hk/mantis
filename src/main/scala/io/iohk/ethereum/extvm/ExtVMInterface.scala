package io.iohk.ethereum.extvm

import java.nio.ByteOrder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Framing, Keep, Sink, Source, Tcp}
import akka.util.ByteString
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm._

import scala.util.Try

// scalastyle:off
class ExtVMInterface(blockchainConfig: BlockchainConfig, host: String, port: Int)(implicit system: ActorSystem) extends VM {

  implicit val materializer = ActorMaterializer()

  val connection = Tcp().outgoingConnection(host, port)

  val (out, in) = Source.queue[ByteString](1024, OverflowStrategy.dropTail)
    .via(connection)
    .via(Framing.lengthField(4, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
    .map(_.drop(4))
    .toMat(Sink.queue[ByteString]())(Keep.both)
    .run()

  override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    new VMClient(blockchainConfig, context, in, out).run()
  }

  def close(): Unit = {
    Try(in.cancel())
    Try(out.complete())
  }

}
