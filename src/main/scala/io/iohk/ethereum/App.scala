package io.iohk.ethereum

import akka.actor.ActorSystem
import akka.agent._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncController
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.dataSource.{LevelDBDataSource, LevelDbConfig}
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.{PeerActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global

object App {

  import Config.{Network => NetworkConfig}

  val nodeKey: AsymmetricCipherKeyPair = generateKeyPair()

  def main(args: Array[String]): Unit = {

    val config = new LevelDbConfig {
      override val cacheSize: Int = 200
      override val verifyChecksums: Boolean = true
      override val paranoidChecks: Boolean = false
      override val createIfMissing: Boolean = true
    }

    val ds = LevelDBDataSource("/tmp",config)

    val storage = PeerActor.Storage(
      new BlockHeadersStorage(ds, new BlockHeadersNumbersStorage(ds)),
      new BlockBodiesStorage(ds),
      new ReceiptStorage(ds),
      new MptNodeStorage(ds),
      new EvmCodeStorage(ds))

    val actorSystem = ActorSystem("etc-client_system")

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening,
        blockchainStatus = BlockchainStatus(Config.Blockchain.genesisDifficulty, Config.Blockchain.genesisHash, 0))

    val nodeStatusHolder = Agent(nodeStatus)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(nodeStatusHolder, storage), "peer-manager")
    val server = actorSystem.actorOf(ServerActor.props(nodeStatusHolder, peerManager), "server")

    server ! ServerActor.StartServer(NetworkConfig.Server.listenAddress)

    val fastSyncController = actorSystem.actorOf(FastSyncController.props(
      peerManager,
      nodeStatusHolder,
      new MptNodeStorage(ds),
      new BlockHeadersStorage(ds, new BlockHeadersNumbersStorage(ds)),
      new BlockBodiesStorage(ds),
      new ReceiptStorage(ds),
      new EvmCodeStorage(ds)), "fast-sync-controller")

    fastSyncController ! FastSyncController.StartFastSync(ByteString(Hex.decode("81e2dcb132c2af3cb84591466aa904bb054f0b9ba52e369c06a271f6d92190db")))
  }

}
