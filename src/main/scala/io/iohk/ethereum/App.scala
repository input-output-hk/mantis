package io.iohk.ethereum

import java.net.URI

import akka.actor.ActorSystem
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{ServerActor, PeerManagerActor}
import io.iohk.ethereum.utils.Config

object App {

  import Config.{Network => NetworkConfig}

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem("etc-client_system")

    val nodeStatusHolder = actorSystem.actorOf(NodeStatusHolder.props(nodeKey), "node-status-holder")
    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeKey, nodeStatusHolder), "peer-manager")
    val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

    server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)

    val bootstrapNodes = NetworkConfig.Discovery.bootstrapNodes.map(new URI(_))
    bootstrapNodes.foreach { node =>
      peerManager ! PeerManagerActor.ConnectToPeer(node)
    }
  }
}
