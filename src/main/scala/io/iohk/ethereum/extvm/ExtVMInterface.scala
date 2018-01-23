package io.iohk.ethereum.extvm

import java.nio.ByteOrder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Framing, Keep, Sink, Source, Tcp}
import akka.util.ByteString
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm._

import scala.util.Try

class ExtVMInterface(host: String, port: Int, blockchainConfig: BlockchainConfig)(implicit system: ActorSystem) extends VM {

  private implicit val materializer = ActorMaterializer()

  private val connection = Tcp().outgoingConnection(host, port)

  private val (out, in) = Source.queue[ByteString](QueueBufferSize, OverflowStrategy.dropTail)
    .via(connection)
    .via(Framing.lengthField(LengthPrefixSize, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
    .map(_.drop(4))
    .toMat(Sink.queue[ByteString]())(Keep.both)
    .run()

  val vmClient = new VMClient(in, out)

  vmClient.setBlockchainConfig(blockchainConfig)

  override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    vmClient.run(context)
  }

  def close(): Unit = {
    Try(in.cancel())
    Try(out.complete())
  }

}
