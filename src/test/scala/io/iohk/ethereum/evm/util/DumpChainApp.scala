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
      bootstrapNodes = Set("enode://d5cfb569e8be4b178ccdfea66fa893c272c853c262b009840df5d35b0422617d526ceb518a00f4e25d9f0e0dc03fa3129e878475a6bb73717cf636550f499c09@192.168.1.105:30303")),
      "peer-manager")
    actorSystem.actorOf(DumpChainActor.props(peerManager), "dumper")
  }
}
