package io.iohk.ethereum

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.agent._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.sync.FastSyncController
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.dataSource.EphemDataSource
import io.iohk.ethereum.db.storage._
import io.iohk.ethereum.network.PeerManagerActor.PeersResponse
import io.iohk.ethereum.network.{PeerActor, PeerManagerActor, ServerActor}
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import org.spongycastle.crypto.AsymmetricCipherKeyPair
import org.spongycastle.util.encoders.Hex

object App {

  val nodeKey = generateKeyPair()

  def main(args: Array[String]): Unit = {
    val actorSystem = ActorSystem("etc-client_system")

    val appActor = actorSystem.actorOf(AppActor.props(nodeKey, actorSystem), "server")

    appActor ! AppActor.StartApp
  }

}

class AppActor(nodeKey: AsymmetricCipherKeyPair, actorSystem: ActorSystem) extends Actor {

  import Config.{Network => NetworkConfig}
  import AppActor._

  override def receive: Receive = idle

  def idle: Receive = {

    case StartApp =>
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

      val storages = Storages(
        mptNodeStorage = new MptNodeStorage(dataSource),
        blockHeadersStorage = new BlockHeadersStorage(dataSource),
        blockBodiesStorage = new BlockBodiesStorage(dataSource),
        receiptStorage = new ReceiptStorage(dataSource),
        evmCodeStorage = new EvmCodeStorage(dataSource),
        totalDifficultyStorage = new TotalDifficultyStorage(dataSource)
      )

      val fastSyncController = actorSystem.actorOf(FastSyncController.props(
        peerManager,
        nodeStatusHolder,
        storages.mptNodeStorage,
        storages.blockHeadersStorage,
        storages.blockBodiesStorage,
        storages.receiptStorage,
        storages.evmCodeStorage), "fast-sync-controller")

      fastSyncController ! FastSyncController.StartFastSync(ByteString(Hex.decode("81e2dcb132c2af3cb84591466aa904bb054f0b9ba52e369c06a271f6d92190db")))

      context watch fastSyncController
      context become waitingForFastSyncDone(storages, peerManager)

  }

  def waitingForFastSyncDone(storages: Storages, peerManager: ActorRef): Receive = {
    case FastSyncController.FastSyncDone =>
      //Ask for peers to start block broadcast
      peerManager ! PeerManagerActor.GetPeers

    case PeersResponse(peers) => peers.foreach{ peer =>
      peer.ref ! PeerActor.StartBlockBroadcast(storages.blockHeadersStorage, storages.blockBodiesStorage, storages.totalDifficultyStorage)
    }
  }

}

object AppActor {
  def props(nodeKey: AsymmetricCipherKeyPair, actorSystem: ActorSystem): Props = {
    Props(new AppActor(nodeKey, actorSystem))
  }

  case object StartApp

  case class Storages(mptNodeStorage: MptNodeStorage,
                      blockHeadersStorage: BlockHeadersStorage,
                      blockBodiesStorage: BlockBodiesStorage,
                      receiptStorage: ReceiptStorage,
                      evmCodeStorage: EvmCodeStorage,
                      totalDifficultyStorage: TotalDifficultyStorage)
}
