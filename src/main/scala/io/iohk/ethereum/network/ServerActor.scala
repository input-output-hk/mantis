package io.iohk.ethereum.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp.{Bind, Bound, CommandFailed, Connected}
import akka.io.{IO, Tcp}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.crypto.params.ECPublicKeyParameters
import org.spongycastle.util.encoders.Hex

class ServerActor(nodeKey: AsymmetricCipherKeyPair, peerManager: ActorRef) extends Actor with ActorLogging {

  import ServerActor._
  import context.system

  override def receive: Receive = {
    case StartServer(address) =>
      IO(Tcp) ! Bind(self, address)
      context become waitingForBindingResult
  }

  def waitingForBindingResult: Receive = {
    case Bound(localAddress) =>
      log.info("Listening on {}", localAddress)
      log.info("Node address: enode://{}@{}:{}", Hex.toHexString(nodeKey.getPublic.asInstanceOf[ECPublicKeyParameters].toNodeId), localAddress.getAddress.getHostAddress, localAddress.getPort)
      context become listening

    case CommandFailed(b: Bind) =>
      log.warning("Binding to {} failed", b.localAddress)
      context stop self
  }

  def listening: Receive = {
    case Connected(remoteAddress, localAddress) =>
      val connection = sender()
      peerManager ! PeerManagerActor.HandlePeerConnection(connection, remoteAddress)
  }
}

object ServerActor {
  def props(nodeKey: AsymmetricCipherKeyPair, peerManager: ActorRef) = Props(new ServerActor(nodeKey, peerManager))

  case class StartServer(address: InetSocketAddress)
}
