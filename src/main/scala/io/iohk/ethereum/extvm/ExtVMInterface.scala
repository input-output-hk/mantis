package io.iohk.ethereum.extvm

import akka.actor.ActorSystem
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Framing, Keep, Sink, SinkQueueWithCancel, Source, SourceQueueWithComplete, Tcp}
import akka.util.ByteString
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.utils.{BlockchainConfig, VmConfig}
import io.iohk.ethereum.vm._

import java.nio.ByteOrder
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class ExtVMInterface(externaVmConfig: VmConfig.ExternalConfig, blockchainConfig: BlockchainConfig, testMode: Boolean)(
    implicit system: ActorSystem
) extends VM[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage] {

  private var vmClient: Option[VMClient] = None

  initConnection()

  private def initConnection(): Unit = {
    close()

    val connection = Tcp().outgoingConnection(externaVmConfig.host, externaVmConfig.port)

    val (connOut, connIn) = Source
      .queue[ByteString](QueueBufferSize, OverflowStrategy.dropTail)
      .via(connection)
      .via(Framing.lengthField(LengthPrefixSize, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
      .map(_.drop(4))
      .toMat(Sink.queue[ByteString]())(Keep.both)
      .run()

    val client = new VMClient(externaVmConfig, new MessageHandler(connIn, connOut), testMode)
    client.sendHello(ApiVersionProvider.version, blockchainConfig)
    //TODO: await hello response, check version

    vmClient = Some(client)
  }

  @tailrec
  final override def run(context: PC): PR = {
    if (vmClient.isEmpty) initConnection()

    Try(vmClient.get.run(context)) match {
      case Success(res) => res
      case Failure(ex) =>
        ex.printStackTrace()
        initConnection()
        run(context)
    }
  }

  def close(): Unit = {
    vmClient.foreach(_.close())
    vmClient = None
  }

}
