package io.iohk.ethereum

import java.net.{InetSocketAddress, URI}

import akka.actor.ActorSystem
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{NodeInfo, PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.Config

object App {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val listenAddress = new InetSocketAddress(Config.Server.interface, Config.Server.port)
    val nodeInfo = NodeInfo(nodeKey, listenAddress)

    val actorSystem = ActorSystem("etc-client_system")

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeInfo))
    val server = actorSystem.actorOf(ServerActor.props(nodeInfo, peerManager))

    server ! ServerActor.StartServer(listenAddress)

    val bootstrapNodes = Config.Discovery.bootstrapNodes.map(new URI(_))
    bootstrapNodes.foreach { node =>
      peerManager ! PeerManagerActor.ConnectToPeer(node)
      Thread.sleep(5 * 1000)
      peerManager ! PeerManagerActor.StartFastDownload(node)
    }
  }
}
