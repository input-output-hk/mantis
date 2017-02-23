package io.iohk.ethereum

import java.io.{File, PrintWriter}

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.agent._
import io.iohk.ethereum.blockchain.sync.FastSyncController
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import io.iohk.ethereum.network._

import scala.io.Source

object App {

  import Config.{Network => NetworkConfig}

  val nodeKey =
    if(!new File(Config.keysFile).exists()){
      val keysValuePair = generateKeyPair()

      //Write keys to file
      val (pub, priv) = getSerialized(keysValuePair)
      val file = new PrintWriter(Config.keysFile)
      file.write(pub ++ "\n" ++ priv)
      file.close()

      keysValuePair
    } else {
      val List(pub, priv) = Source.fromFile(Config.keysFile).getLines().toList
      fromSerialized(pub, priv)
    }

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem("etc-client_system")

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening,
        blockchainStatus = BlockchainStatus(Config.Blockchain.genesisDifficulty, Config.Blockchain.genesisHash, 0))

    val nodeStatusHolder = Agent(nodeStatus)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeStatusHolder), "peer-manager")
    val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

    server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)

    val dataSource = EphemDataSource()
    val fastSyncController = actorSystem.actorOf(FastSyncController.props(
      peerManager,
      nodeStatusHolder,
      new MptNodeStorage(dataSource),
      new BlockHeadersStorage(dataSource),
      new BlockBodiesStorage(dataSource),
      new ReceiptStorage(dataSource),
      new EvmCodeStorage(dataSource)), "fast-sync-controller")

    fastSyncController ! FastSyncController.StartFastSync
  }

}
