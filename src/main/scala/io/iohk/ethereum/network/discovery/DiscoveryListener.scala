package io.iohk.ethereum.network.discovery

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.{IO, Udp}
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference
import scala.util.{Failure, Success}

class DiscoveryListener(
    discoveryConfig: DiscoveryConfig,
    nodeStatusHolder: AtomicReference[NodeStatus])
  extends Actor with ActorLogging {

  import DiscoveryListener._
  import context.system

  var subscribers: Set[ActorRef] = Set.empty

  override def receive: Receive = handleSubscribe orElse {
    case Start =>
      IO(Udp) ! Udp.Bind(self, new InetSocketAddress(discoveryConfig.interface, discoveryConfig.port))

    case Udp.Bound(local) =>
      nodeStatusHolder.getAndUpdate(_.copy(discoveryStatus = ServerStatus.Listening(local)))
      context.become(ready(sender()))
  }

  def handleSubscribe: Receive = {
    case Subscribe =>
      subscribers += sender()
  }

  def ready(socket: ActorRef): Receive = handleSubscribe orElse {
    case Udp.Received(data, remote) =>
      val msgReceivedTry = for {
        packet <- decodePacket(data)
        message <- extractMessage(packet)
      } yield MessageReceived(message, remote, packet)

      msgReceivedTry match {
        case Success(msgReceived) => subscribers.foreach(_ ! msgReceived)
        case Failure(ex) => log.error(ex, "Unable to decode discovery packet from {}", remote)
      }

    case SendPacket(packet, to) =>
      socket ! Udp.Send(packet.wire, to)
  }
}

object DiscoveryListener {
  def props(config: DiscoveryConfig, nodeStatusHolder: AtomicReference[NodeStatus]): Props =
    Props(new DiscoveryListener(config, nodeStatusHolder))

  case object Start
  case object Subscribe

  case class SendPacket(packet: Packet, to: InetSocketAddress)
  case class MessageReceived(message: Message, from: InetSocketAddress, packet: Packet)
}
