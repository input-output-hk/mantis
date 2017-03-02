package io.iohk.ethereum

import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.ActorSystem
import akka.agent._
import io.iohk.ethereum.blockchain.sync.FastSyncController
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, _}
import io.iohk.ethereum.domain.{Blockchain, BlockchainImpl}
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.rpc.JsonRpcServer
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}

object App {

  import Config.{Network => NetworkConfig}


  def main(args: Array[String]): Unit = {

    new nodebuilder.App {
      override def shutdown(): Unit = {
        storagesInstance.dataSources.closeAll
        actorSystem.terminate
      }

      server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)
      fastSyncController ! FastSyncController.StartFastSync

      if(rpcServerConfig.enabled) startJSONRpcServer()
    }

  }

}
