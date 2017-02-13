package io.iohk.ethereum

import akka.util.ByteString
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global

import akka.actor.{Props, ActorSystem}
import akka.agent._
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.network.{FastSyncController, ServerActor, PeerManagerActor}
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

    val dataSource = EphemDataSource()
    val fastSyncController = actorSystem.actorOf(Props(new FastSyncController(
      peerManager,
      new MptNodeStorage(dataSource),
      new BlockHeadersStorage(dataSource),
      new BlockBodiesStorage(dataSource),
      new ReceiptStorage(dataSource),
      new EvmCodeStorage(dataSource))))


    fastSyncController ! FastSyncController.StartFastSync(ByteString(Hex.decode("dc550c0c4345df8183f77fd52773e2ae0c453c288e3371bdfbb7fb0e22f90b4e")))
  }

}
