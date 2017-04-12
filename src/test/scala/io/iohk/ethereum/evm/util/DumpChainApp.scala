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
      bootstrapNodes = Set("enode://9e56c4eb31089d9b6b40c183f0e549ca5be049dbb22f4b0e25f4955e19d580915ddc32ec26ff38693cbf6954ff743478e42a532b37e3c7a87415067d81c019d5@192.168.1.105:30303")),
      "peer-manager")
    actorSystem.actorOf(DumpChainActor.props(peerManager), "dumper")
  }
}
