package io.iohk.ethereum.txExecTest.util

import java.time.Clock
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.util.ByteString

import scala.concurrent.duration._

import com.typesafe.config
import com.typesafe.config.ConfigFactory
import org.bouncycastle.util.encoders.Hex
import org.scalamock.scalatest.MockFactory

import io.iohk.ethereum.blockchain.sync.CacheBasedBlacklist
import io.iohk.ethereum.db.components.RocksDbDataSourceComponent
import io.iohk.ethereum.db.components.Storages
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.storage.AppStateStorage
import io.iohk.ethereum.db.storage.MptStorage
import io.iohk.ethereum.db.storage.NodeStorage.NodeEncoded
import io.iohk.ethereum.db.storage.NodeStorage.NodeHash
import io.iohk.ethereum.db.storage.pruning.ArchivePruning
import io.iohk.ethereum.db.storage.pruning.PruningMode
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefEmpty
import io.iohk.ethereum.domain.Blockchain
import io.iohk.ethereum.domain._
import io.iohk.ethereum.domain.branch.BlockchainBranch
import io.iohk.ethereum.jsonrpc.ProofService.EmptyStorageValueProof
import io.iohk.ethereum.jsonrpc.ProofService.StorageProof
import io.iohk.ethereum.jsonrpc.ProofService.StorageProofKey
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.ledger.InMemoryWorldStateProxyStorage
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.ForkResolver
import io.iohk.ethereum.network.PeerEventBusActor
import io.iohk.ethereum.network.PeerManagerActor
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.PeerStatisticsActor
import io.iohk.ethereum.network.discovery.DiscoveryConfig
import io.iohk.ethereum.network.handshaker.EtcHandshaker
import io.iohk.ethereum.network.handshaker.EtcHandshakerConfiguration
import io.iohk.ethereum.network.handshaker.Handshaker
import io.iohk.ethereum.network.p2p.messages.Capability
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.nodebuilder.AuthHandshakerBuilder
import io.iohk.ethereum.nodebuilder.NodeKeyBuilder
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.Config
import io.iohk.ethereum.utils.NodeStatus
import io.iohk.ethereum.utils.ServerStatus

object DumpChainApp
    extends App
    with NodeKeyBuilder
    with SecureRandomBuilder
    with AuthHandshakerBuilder
    with MockFactory {
  val conf: config.Config = ConfigFactory.load("txExecTest/chainDump.conf")
  val node: String = conf.getString("node")
  val genesisHash: ByteString = ByteString(Hex.decode(conf.getString("genesisHash")))
  val privateNetworkId: Int = conf.getInt("networkId")
  val startBlock: Int = conf.getInt("startBlock")
  val maxBlocks: Int = conf.getInt("maxBlocks")

  val blockchainConfig = Config.blockchains.blockchainConfig
  val discoveryConfig: DiscoveryConfig = DiscoveryConfig(Config.config, blockchainConfig.bootstrapNodes)

  val peerConfig: PeerConfiguration = new PeerConfiguration {
    override val rlpxConfiguration: RLPxConfiguration = Config.Network.peer.rlpxConfiguration
    override val connectRetryDelay: FiniteDuration = Config.Network.peer.connectRetryDelay
    override val connectMaxRetries: Int = Config.Network.peer.connectMaxRetries
    override val disconnectPoisonPillTimeout: FiniteDuration = Config.Network.peer.disconnectPoisonPillTimeout
    override val waitForHelloTimeout: FiniteDuration = Config.Network.peer.waitForHelloTimeout
    override val waitForStatusTimeout: FiniteDuration = Config.Network.peer.waitForStatusTimeout
    override val waitForChainCheckTimeout: FiniteDuration = Config.Network.peer.waitForChainCheckTimeout
    override val fastSyncHostConfiguration: PeerManagerActor.FastSyncHostConfiguration =
      Config.Network.peer.fastSyncHostConfiguration
    override val minOutgoingPeers: Int = Config.Network.peer.minOutgoingPeers
    override val maxOutgoingPeers: Int = Config.Network.peer.maxOutgoingPeers
    override val maxIncomingPeers: Int = Config.Network.peer.maxIncomingPeers
    override val maxPendingPeers: Int = Config.Network.peer.maxPendingPeers
    override val pruneIncomingPeers: Int = Config.Network.peer.pruneIncomingPeers
    override val minPruneAge: FiniteDuration = Config.Network.peer.minPruneAge
    override val networkId: Int = privateNetworkId
    override val updateNodesInitialDelay: FiniteDuration = 5.seconds
    override val updateNodesInterval: FiniteDuration = 20.seconds
    override val shortBlacklistDuration: FiniteDuration = 1.minute
    override val longBlacklistDuration: FiniteDuration = 3.minutes
    override val statSlotDuration: FiniteDuration = 1.minute
    override val statSlotCount: Int = 30
  }

  val actorSystem: ActorSystem = ActorSystem("mantis_system")
  trait PruningConfig extends PruningModeComponent {
    override val pruningMode: PruningMode = ArchivePruning
  }
  val storagesInstance: RocksDbDataSourceComponent with PruningConfig with Storages.DefaultStorages =
    new RocksDbDataSourceComponent with PruningConfig with Storages.DefaultStorages

  val blockchain: Blockchain = new BlockchainMock(genesisHash)
  val blockchainReader: BlockchainReader = mock[BlockchainReader]
  val bestChain: BlockchainBranch = mock[BlockchainBranch]
  (blockchainReader.getBestBranch _).expects().anyNumberOfTimes().returning(Some(bestChain))
  (bestChain.getHashByBlockNumber _).expects(*).returning(Some(genesisHash))

  val nodeStatus: NodeStatus =
    NodeStatus(key = nodeKey, serverStatus = ServerStatus.NotListening, discoveryStatus = ServerStatus.NotListening)

  lazy val nodeStatusHolder = new AtomicReference(nodeStatus)

  lazy val forkResolverOpt: Option[ForkResolver.EtcForkResolver] =
    blockchainConfig.daoForkConfig.map(new ForkResolver.EtcForkResolver(_))

  private val handshakerConfiguration: EtcHandshakerConfiguration =
    new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = DumpChainApp.forkResolverOpt
      override val nodeStatusHolder: AtomicReference[NodeStatus] = DumpChainApp.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = peerConfig
      override val blockchain: Blockchain = DumpChainApp.blockchain
      override val blockchainReader: BlockchainReader = DumpChainApp.blockchainReader
      override val appStateStorage: AppStateStorage = storagesInstance.storages.appStateStorage
      override val capabilities: List[Capability] = blockchainConfig.capabilities
    }

  lazy val handshaker: Handshaker[PeerInfo] = EtcHandshaker(handshakerConfiguration)

  val peerMessageBus: ActorRef = actorSystem.actorOf(PeerEventBusActor.props)

  val peerStatistics: ActorRef =
    actorSystem.actorOf(PeerStatisticsActor.props(peerMessageBus, 1.minute, 30)(Clock.systemUTC()))

  val blacklist: CacheBasedBlacklist = CacheBasedBlacklist.empty(100)

  val peerManager: ActorRef = actorSystem.actorOf(
    PeerManagerActor.props(
      peerDiscoveryManager = actorSystem.deadLetters, // TODO: fixme
      peerConfiguration = peerConfig,
      peerMessageBus = peerMessageBus,
      peerStatistics = peerStatistics,
      knownNodesManager = actorSystem.deadLetters, // TODO: fixme
      handshaker = handshaker,
      authHandshaker = authHandshaker,
      discoveryConfig = discoveryConfig,
      blacklist = blacklist,
      capabilities = blockchainConfig.capabilities
    ),
    "peer-manager"
  )
  peerManager ! PeerManagerActor.StartConnecting

  actorSystem.actorOf(DumpChainActor.props(peerManager, peerMessageBus, maxBlocks, node), "dumper")
}

class BlockchainMock(genesisHash: ByteString) extends Blockchain {

  class FakeHeader()
      extends BlockHeader(
        ByteString.empty,
        ByteString.empty,
        ByteString.empty,
        ByteString.empty,
        ByteString.empty,
        ByteString.empty,
        ByteString.empty,
        0,
        0,
        0,
        0,
        0,
        ByteString.empty,
        ByteString.empty,
        ByteString.empty,
        HefEmpty
      ) {
    override lazy val hash: ByteString = genesisHash
  }

  override def getAccountProof(address: Address, blockNumber: BigInt): Option[Vector[MptNode]] = None

  override def getStorageProofAt(
      rootHash: NodeHash,
      position: BigInt,
      ethCompatibleStorage: Boolean
  ): StorageProof = EmptyStorageValueProof(StorageProofKey(position))

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit = ???

  override def removeBlock(hash: ByteString, withState: Boolean = true): Unit = ???

  override def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight] = ???

  def getAccount(address: Address, blockNumber: BigInt): Option[Account] = ???

  override def getAccountStorageAt(rootHash: ByteString, position: BigInt, ethCompatibleStorage: Boolean): ByteString =
    ???

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy

  def getBestBlockNumber(): BigInt = ???

  def saveBlockNumber(number: BigInt, hash: NodeHash): Unit = ???

  def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit = ???

  def getBestBlock(): Option[Block] = ???

  override def getLatestCheckpointBlockNumber(): BigInt = ???

  override def isInChain(hash: NodeHash): Boolean = ???

  override def getBackingMptStorage(blockNumber: BigInt): MptStorage = ???

  override def getReadOnlyMptStorage(): MptStorage = ???
}
