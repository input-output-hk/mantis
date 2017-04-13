package io.iohk.ethereum.transactionTest.util

import akka.actor.ActorSystem
import akka.agent.Agent
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.db.components.{SharedLevelDBDataSources, Storages}
import io.iohk.ethereum.domain.{Blockchain, _}
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.p2p.messages.{PV62, PV63}
import io.iohk.ethereum.network.{PeerManagerActor, loadAsymmetricCipherKeyPair}
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import org.spongycastle.util.encoders.Hex

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration

object DumpChainApp {
  // scalastyle:off
  def main(args: Array[String]): Unit = {

    val conf = ConfigFactory.load("chainDump.conf")
    val node = conf.getString("node")
    val genesisHash = ByteString(Hex.decode(conf.getString("genesisHash")))
    val privateNetworkId = conf.getInt("networkId")
    val startBlock = conf.getInt("startBlock")
    val maxBlocks = conf.getInt("maxBlocks")

    val peerConfig = new PeerConfiguration {
      override val connectRetryDelay: FiniteDuration = Config.Network.peer.connectRetryDelay
      override val connectMaxRetries: Int = Config.Network.peer.connectMaxRetries
      override val disconnectPoisonPillTimeout: FiniteDuration = Config.Network.peer.disconnectPoisonPillTimeout
      override val waitForStatusTimeout: FiniteDuration = Config.Network.peer.waitForStatusTimeout
      override val waitForChainCheckTimeout: FiniteDuration = Config.Network.peer.waitForChainCheckTimeout
      override val fastSyncHostConfiguration: PeerManagerActor.FastSyncHostConfiguration = Config.Network.peer.fastSyncHostConfiguration
      override val maxPeers: Int = Config.Network.peer.maxPeers
      override val networkId: Int = privateNetworkId
    }

    val actorSystem = ActorSystem("etc-client_system")
    val storagesInstance = new SharedLevelDBDataSources with Storages.DefaultStorages

    val blockchain: Blockchain = new BlockchainMock(genesisHash)

    val nodeKey = loadAsymmetricCipherKeyPair(Config.keysFile)

    val nodeStatus =
      NodeStatus(
        key = nodeKey,
        serverStatus = ServerStatus.NotListening)

    lazy val nodeStatusHolder = Agent(nodeStatus)

    val peerManager = actorSystem.actorOf(PeerManagerActor.props(
      nodeStatusHolder = nodeStatusHolder,
      peerConfiguration = peerConfig,
      appStateStorage = storagesInstance.storages.appStateStorage,
      blockchain = blockchain,
      bootstrapNodes = Set(node)), "peer-manager")
    actorSystem.actorOf(DumpChainActor.props(peerManager,startBlock,maxBlocks), "dumper")
  }

  class BlockchainMock(genesisHash: ByteString) extends Blockchain {

    class FakeHeader() extends BlockHeader(ByteString.empty, ByteString.empty, ByteString.empty, ByteString.empty, ByteString.empty, ByteString.empty, ByteString.empty, 0, 0, 0, 0, 0, ByteString.empty, ByteString.empty, ByteString.empty) {
      override lazy val hash: ByteString = genesisHash
    }

    override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] = Some(genesisHash)

    override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] = Some(new FakeHeader())

    override def getBlockBodyByHash(hash: ByteString): Option[PV62.BlockBody] = ???

    override def getMptNodeByHash(hash: ByteString): Option[PV63.MptNode] = ???

    override def save(blockHeader: BlockHeader): Unit = ???

    override def save(blockHash: ByteString, blockBody: PV62.BlockBody): Unit = ???

    override def save(blockHash: ByteString, receipts: Seq[Receipt]): Unit = ???

    override def save(hash: ByteString, evmCode: ByteString): Unit = ???

    override def save(node: PV63.MptNode): Unit = ???

    override def save(blockhash: ByteString, totalDifficulty: BigInt): Unit = ???

    override def removeBlock(hash: ByteString): Unit = ???

    override def getTotalDifficultyByHash(blockhash: ByteString): Option[BigInt] = ???

    override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = ???

    override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = ???
  }

}
