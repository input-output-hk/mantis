package io.iohk.ethereum

import java.net.URI

import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.ActorSystem
import akka.agent._
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{ServerActor, PeerManagerActor}
import io.iohk.ethereum.utils.{BlockchainStatus, ServerStatus, NodeStatus, Config}

object App {

  import Config.{Network => NetworkConfig}

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem("etc-client_system")

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening,
        blockchainStatus = BlockchainStatus(Config.Blockchain.genesisDifficulty, Config.Blockchain.genesisHash))

    val nodeStatusHolder = Agent(nodeStatus)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeStatusHolder), "peer-manager")
    val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

    server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)

    val bootstrapNodes = NetworkConfig.Discovery.bootstrapNodes.map(new URI(_))
    bootstrapNodes.foreach { node =>
      peerManager ! PeerManagerActor.ConnectToPeer(node)
    }
  }

}
