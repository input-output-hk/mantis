package io.iohk.ethereum.network

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import akka.io.IO
import akka.io.Tcp
import akka.io.Tcp.Bind
import akka.io.Tcp.Bound
import akka.io.Tcp.CommandFailed
import akka.io.Tcp.Connected
import io.iohk.ethereum.utils.NodeStatus
import io.iohk.ethereum.utils.ServerStatus
import org.bouncycastle.util.encoders.Hex

import java.net.InetSocketAddress
import java.util.concurrent.atomic.AtomicReference

class ServerActor(nodeStatusHolder: AtomicReference[NodeStatus], peerManager: ActorRef)
    extends Actor
    with ActorLogging {

  import ServerActor._
  import context.system

  override def receive: Receive = { case StartServer(address) =>
    IO(Tcp) ! Bind(self, address)
    context.become(waitingForBindingResult)
  }

  def waitingForBindingResult: Receive = {
    case Bound(localAddress) =>
      val nodeStatus = nodeStatusHolder.get()
      log.info("Listening on {}", localAddress)
      log.info(
        "Node address: enode://{}@{}:{}",
        Hex.toHexString(nodeStatus.nodeId),
        getHostName(localAddress.getAddress),
        localAddress.getPort
      )
      nodeStatusHolder.getAndUpdate(_.copy(serverStatus = ServerStatus.Listening(localAddress)))
      context.become(listening)

    case CommandFailed(b: Bind) =>
      log.warning("Binding to {} failed", b.localAddress)
      context.stop(self)
  }

  def listening: Receive = { case Connected(remoteAddress, _) =>
    val connection = sender()
    peerManager ! PeerManagerActor.HandlePeerConnection(connection, remoteAddress)
  }
}

object ServerActor {
  def props(nodeStatusHolder: AtomicReference[NodeStatus], peerManager: ActorRef): Props =
    Props(new ServerActor(nodeStatusHolder, peerManager))

  case class StartServer(address: InetSocketAddress)
}
