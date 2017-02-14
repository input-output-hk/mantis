package io.iohk.ethereum

import java.net.URI

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Props, ActorSystem}
import akka.agent._
import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.{FastSyncController, FastSyncActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import org.spongycastle.util.encoders.Hex

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
      nodeStatusHolder,
      new MptNodeStorage(dataSource),
      new BlockHeadersStorage(dataSource),
      new BlockBodiesStorage(dataSource),
      new ReceiptStorage(dataSource),
      new EvmCodeStorage(dataSource))))


    fastSyncController ! FastSyncController.StartFastSync(ByteString(Hex.decode("6a97918a39123ead4673c4ef0a88f77765f5c15c87d0faa906b95cb2cc729784")))
  }

}
