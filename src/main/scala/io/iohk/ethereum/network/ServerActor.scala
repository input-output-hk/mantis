package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.agent.Agent
import akka.io.Tcp.{Bind, Bound, CommandFailed, Connected}
import akka.io.{IO, Tcp}
import io.iohk.ethereum.network.PeerManagerActor.HandlePeerConnection
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus}
import org.spongycastle.util.encoders.Hex

class ServerActor(nodeStatusHolder: Agent[NodeStatus], peerManagerActor: ActorRef) extends Actor with ActorLogging {

  import ServerActor._
  import context.system

  override def receive: Receive = {
    case StartServer(address) =>
      IO(Tcp) ! Bind(self, address)
      context become waitingForBindingResult
  }

  def waitingForBindingResult: Receive = {
    case Bound(localAddress) =>
      val nodeStatus = nodeStatusHolder()
      log.info("Listening on {}", localAddress)
      log.info("Node address: enode://{}@{}:{}",
        Hex.toHexString(nodeStatus.nodeId),
        localAddress.getAddress.getHostAddress,
        localAddress.getPort)
      nodeStatusHolder.send(_.copy(serverStatus = ServerStatus.Listening(localAddress)))
      context become listening

    case CommandFailed(b: Bind) =>
      log.warning("Binding to {} failed", b.localAddress)
      context stop self

    case _: StartServer =>
      log.warning("Trying to start server but it cannot start as it was already started")
  }

  def listening: Receive = {
    case Connected(remoteAddress, _) =>
      val connection = sender()
      peerManagerActor ! HandlePeerConnection(connection, remoteAddress)

    case _: StartServer =>
      log.warning("Trying to start server but it cannot start as it was already started")
  }
}

object ServerActor {
  def props(nodeStatusHolder: Agent[NodeStatus], peerManagerActor: ActorRef): Props =
    Props(new ServerActor(nodeStatusHolder, peerManagerActor))

  case class StartServer(address: InetSocketAddress)
}
