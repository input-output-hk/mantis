package io.iohk.ethereum.network

import cats.effect.Resource
import cats.implicits._
import io.iohk.ethereum.utils.Logger
import monix.eval.Task
import org.jupnp.DefaultUpnpServiceConfiguration
import org.jupnp.QueueingThreadPoolExecutor
import org.jupnp.UpnpService
import org.jupnp.UpnpServiceImpl
import org.jupnp.support.igd.PortMappingListener
import org.jupnp.support.model.PortMapping
import org.jupnp.support.model.PortMapping.Protocol.TCP
import org.jupnp.support.model.PortMapping.Protocol.UDP
import org.jupnp.tool.transport.JDKTransportConfiguration
import org.jupnp.transport.Router
import org.jupnp.transport.spi.NetworkAddressFactory
import org.jupnp.transport.spi.StreamClient
import org.jupnp.transport.spi.StreamClientConfiguration
import org.jupnp.transport.spi.StreamServer
import org.jupnp.transport.spi.StreamServerConfiguration

import java.net.InetAddress
import java.util.concurrent.ExecutorService
import scala.jdk.CollectionConverters._
import scala.util.chaining._

private class ClientOnlyUpnpServiceConfiguration extends DefaultUpnpServiceConfiguration() {
  final private val THREAD_POOL_SIZE = 4 // seemingly the minimum required to perform port mapping

  override def createDefaultExecutorService(): ExecutorService =
    QueueingThreadPoolExecutor.createInstance("mantis-jupnp", THREAD_POOL_SIZE);

  override def createStreamClient(): StreamClient[_ <: StreamClientConfiguration] =
    JDKTransportConfiguration.INSTANCE.createStreamClient(getSyncProtocolExecutorService())

  override def createStreamServer(networkAddressFactory: NetworkAddressFactory): NoStreamServer.type =
    NoStreamServer // prevent a StreamServer from running needlessly
}

private object NoStreamServer extends StreamServer[StreamServerConfiguration] {
  def run(): Unit = ()
  def init(_1: InetAddress, _2: Router): Unit = ()
  def getPort(): Int = 0
  def stop(): Unit = ()
  def getConfiguration(): StreamServerConfiguration = new StreamServerConfiguration {
    def getListenPort(): Int = 0
  }
}

object PortForwarder extends Logger {
  final private val description = "Mantis"

  def openPorts(tcpPorts: Seq[Int], udpPorts: Seq[Int]): Resource[Task, Unit] =
    Resource.make(startForwarding(tcpPorts, udpPorts))(stopForwarding).void

  private def startForwarding(tcpPorts: Seq[Int], udpPorts: Seq[Int]): Task[UpnpService] = Task {
    log.info("Attempting port forwarding for TCP ports {} and UDP ports {}", tcpPorts, udpPorts)
    new UpnpServiceImpl(new ClientOnlyUpnpServiceConfiguration()).tap { service =>
      service.startup()

      val bindAddresses =
        service
          .getConfiguration()
          .createNetworkAddressFactory()
          .getBindAddresses()
          .asScala
          .map(_.getHostAddress())
          .toArray

      val portMappings = for {
        address <- bindAddresses
        (port, protocol) <- tcpPorts.map(_ -> TCP) ++ udpPorts.map(_ -> UDP)
      } yield new PortMapping(port, address, protocol).tap(_.setDescription(description))

      service.getRegistry().addListener(new PortMappingListener(portMappings))
    }
  }

  private def stopForwarding(service: UpnpService) = Task {
    service.shutdown()
  }
}
