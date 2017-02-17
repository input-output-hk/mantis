package io.iohk.ethereum

import java.net.URI

import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.ActorSystem
import akka.agent._
import akka.util.ByteString
import io.iohk.ethereum.blockchain.{BlockchainComp, BlockchainCompImpl}
import io.iohk.ethereum.crypto._
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, _}
import io.iohk.ethereum.network.{PeerManagerActor, ServerActor}
import io.iohk.ethereum.rpc.JsonRpcServer
import io.iohk.ethereum.utils.{BlockchainStatus, Config, NodeStatus, ServerStatus}
import org.spongycastle.util.encoders.Hex

object App {

  import Config.{Network => NetworkConfig}

  val nodeKey = generateKeyPair()

  // FIXME We need to close DataSources before app ends
  val storagesInstance =  new Storages.DefaultStorages with SharedLevelDBDataSources
  val blockchainComp: BlockchainComp = new BlockchainCompImpl {
    override val storagesComp: StoragesComp = storagesInstance
  }

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

    if(Config.Network.Rpc.enabled) JsonRpcServer.run(actorSystem, blockchainComp.blockchain, Config.Network.Rpc)

    val bootstrapNodes = NetworkConfig.Discovery.bootstrapNodes.map(new URI(_))
    bootstrapNodes.foreach { node =>
      peerManager ! PeerManagerActor.ConnectToPeer(node)
      //TODO change to CLI command?
      Thread.sleep(2 * 1000)
      peerManager ! PeerManagerActor.StartFastDownload(node,
        ByteString(Hex.decode("12d7b70f28b819867087c37e4190b727dfbb1f7f34e2687e5ad126a31cf051be")),
        blockchainComp.blockchain,
        storagesInstance.storages.mptNodeStorage
      )
    }

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        storagesInstance.dataSources.closeAll
      }
    })
  }

}
