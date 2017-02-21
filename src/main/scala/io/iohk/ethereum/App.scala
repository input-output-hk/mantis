package io.iohk.ethereum

import java.net.URI

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.agent._
import akka.util.ByteString
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.{FastSyncActor, PeerActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import org.spongycastle.util.encoders.Hex

object App {

  import Config.{Network => NetworkConfig}

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {

    val config = new LevelDbConfig {
      override val cacheSize: Int = 200
      override val verifyChecksums: Boolean = true
      override val paranoidChecks: Boolean = false
      override val createIfMissing: Boolean = true
    }

    val ds = LevelDBDataSource("/tmp",config)

    val storage = FastSyncActor.Storage(
      new BlockHeadersStorage(ds, new BlockHeadersNumbersStorage(ds)),
      new BlockBodiesStorage(ds),
      new ReceiptStorage(ds),
      new MptNodeStorage(ds),
      new EvmCodeStorage(ds)
    )

    val actorSystem = ActorSystem("etc-client_system")

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening,
        blockchainStatus = BlockchainStatus(Config.Blockchain.genesisDifficulty, Config.Blockchain.genesisHash))

    val nodeStatusHolder = Agent(nodeStatus)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeStatusHolder, storage), "peer-manager")
    val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

    server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)

    val bootstrapNodes = NetworkConfig.Discovery.bootstrapNodes.map(new URI(_))
    bootstrapNodes.foreach { node =>
      peerManager ! PeerManagerActor.ConnectToPeer(node)
      //TODO change to CLI command?
      Thread.sleep(2 * 1000)
      peerManager ! PeerManagerActor.StartFastDownload(node,
        ByteString(Hex.decode("6a063566fa4e4d31cd8babe9e0278fb0f3ea36473f3d15cccce871edf1a23854")), storage)
    }
  }

}
