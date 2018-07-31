package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.agent.Agent
import akka.io.Tcp.{Bind, Bound, CommandFailed, Connected}
import akka.io.{IO, Tcp}
import io.iohk.ethereum.utils.{NodeStatus, ServerStatus, Riemann}
import org.spongycastle.util.encoders.Hex

class ServerActor(nodeStatusHolder: Agent[NodeStatus], peerManager: ActorRef) extends Actor {

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
      Riemann.ok("server actor listening")
        .attribute("localAddress", localAddress.toString)
        .attribute("nodeAddress", s"enode://${Hex.toHexString(nodeStatus.nodeId)}@${getHostName(localAddress.getAddress)}:${localAddress.getPort}")
        .send
      nodeStatusHolder.send(_.copy(serverStatus = ServerStatus.Listening(localAddress)))
      context become listening

    case CommandFailed(b: Bind) =>
      Riemann.error("server actor listening")
        .attribute("localAddress", b.localAddress.toString)
        .send
      context stop self
  }

  def listening: Receive = {
    case Connected(remoteAddress, _) =>
      val connection = sender()
      peerManager ! PeerManagerActor.HandlePeerConnection(connection, remoteAddress)
  }
}

object ServerActor {
  def props(nodeStatusHolder: Agent[NodeStatus], peerManager: ActorRef): Props =
    Props(new ServerActor(nodeStatusHolder, peerManager))

  case class StartServer(address: InetSocketAddress)
}
