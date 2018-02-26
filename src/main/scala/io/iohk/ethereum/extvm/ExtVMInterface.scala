package io.iohk.ethereum.extvm

import java.nio.ByteOrder

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Framing, Keep, Sink, Source, Tcp}
import akka.util.ByteString
import atmos.dsl._
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.VmConfig.ExternalConfig
import io.iohk.ethereum.vm._

class ExtVMInterface(
  externalConfig: ExternalConfig,
  blockchainConfig: BlockchainConfig)
  (implicit system: ActorSystem)
  extends VM[InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage]{

  private implicit val materializer = ActorMaterializer()

  private implicit val retryPolicy = {
    import Slf4jSupport._
    import externalConfig.retry._

    retryFor(times.attempts).using(constantBackoff(delay))
      .monitorWith(log onRetrying logDebug onInterrupted logError onAborted logError)
      .onError {
        case VmDisconnectException(_) =>
          close()
          stopRetrying
      }
  }

  private var vmClient: Option[VMClient] = None

  retry()(initConnection())

  private def initConnection(): Unit = {
    val connection = Tcp().outgoingConnection(externalConfig.host, externalConfig.port)

    val (connOut, connIn) = Source.queue[ByteString](QueueBufferSize, OverflowStrategy.dropTail)
      .via(connection)
      .via(Framing.lengthField(LengthPrefixSize, 0, Int.MaxValue, ByteOrder.BIG_ENDIAN))
      .map(_.drop(4))
      .toMat(Sink.queue[ByteString]())(Keep.both)
      .run()

    val client = new VMClient(connIn, connOut, externalConfig.testMode)
    //TODO: read version from file, recognise different VMs
    client.sendHello("1.1", blockchainConfig)
    //TODO: await hello response, check version

    vmClient = Some(client)
  }

  override final def run(context: PC): PR = {
    retry() {
      if (vmClient.isEmpty) initConnection()
      vmClient.get.run(context)
    }
  }

  def close(): Unit = {
    vmClient.foreach(_.close())
    vmClient = None
  }

}
