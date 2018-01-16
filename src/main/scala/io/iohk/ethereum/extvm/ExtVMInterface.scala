package io.iohk.ethereum.extvm

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Keep, StreamConverters, Tcp}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm._

import scala.util.Try

class ExtVMInterface(blockchainConfig: BlockchainConfig, host: String, port: Int)(implicit system: ActorSystem) extends VM {

  implicit val materializer = ActorMaterializer()

  val connection = Tcp().outgoingConnection(host, port)

  val inSink = StreamConverters.asInputStream()
  val outSource = StreamConverters.asOutputStream()

  val (out, in) = outSource.via(connection).toMat(inSink)(Keep.both).run()

  override def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    new VMClient(blockchainConfig, context, in, out).run()
  }

  def close(): Unit = {
    Try(in.close())
    Try(out.close())
  }

}
