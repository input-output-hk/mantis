package io.iohk.ethereum.evm.util

import akka.actor.ActorSystem
import akka.agent.Agent
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.network.{PeerManagerActor, loadAsymmetricCipherKeyPair}
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}

import scala.concurrent.ExecutionContext.Implicits.global

object DumpChainApp {
  // scalastyle:off
  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem("etc-client_system")
    val storagesInstance = new SharedLevelDBDataSources with Storages.DefaultStorages

    val blockchain: Blockchain = BlockchainImpl(storagesInstance.storages)
    val nodeKey = loadAsymmetricCipherKeyPair(Config.keysFile)

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening)

    lazy val nodeStatusHolder = Agent(nodeStatus)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(
      nodeStatusHolder = nodeStatusHolder,
      peerConfiguration = Config.Network.peer,
      appStateStorage = storagesInstance.storages.appStateStorage,
      blockchain = blockchain,
      bootstrapNodes = Set("enode://83d6145bac540efc635fbf60b94c2f7691aea4416c7dbaef5c08a2c2542d05de17e6c9a7d7462ec1317473aff93c2f90523944969c88a66c45761423494ae3ce@192.168.1.105:30303")),
      "peer-manager")
    actorSystem.actorOf(DumpChainActor.props(peerManager), "dumper")
  }
}
