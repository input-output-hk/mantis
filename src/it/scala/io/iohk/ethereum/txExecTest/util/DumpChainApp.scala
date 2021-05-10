package io.iohk.ethereum.txExecTest.util

import java.time.Clock
import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import akka.util.ByteString
import com.typesafe.config.ConfigFactory
import io.iohk.ethereum.blockchain.sync.CacheBasedBlacklist
import io.iohk.ethereum.db.components.Storages.PruningModeComponent
import io.iohk.ethereum.db.components.{RocksDbDataSourceComponent, Storages}
import io.iohk.ethereum.db.dataSource.{DataSourceBatchUpdate, RocksDbDataSource}
import io.iohk.ethereum.db.storage.NodeStorage.{NodeEncoded, NodeHash}
import io.iohk.ethereum.db.storage.TransactionMappingStorage.TransactionLocation
import io.iohk.ethereum.db.storage.pruning.{ArchivePruning, PruningMode}
import io.iohk.ethereum.db.storage.{AppStateStorage, StateStorage}
import io.iohk.ethereum.domain.BlockHeader.HeaderExtraFields.HefEmpty
import io.iohk.ethereum.domain.{Blockchain, UInt256, _}
import io.iohk.ethereum.jsonrpc.ProofService.{EmptyStorageValueProof, StorageProof, StorageProofKey, StorageValueProof}
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, InMemoryWorldStateProxyStorage}
import io.iohk.ethereum.mpt.MptNode
import io.iohk.ethereum.network.EtcPeerManagerActor.PeerInfo
import io.iohk.ethereum.network.PeerManagerActor.PeerConfiguration
import io.iohk.ethereum.network.PeerStatisticsActor
import io.iohk.ethereum.network.discovery.DiscoveryConfig
import io.iohk.ethereum.network.handshaker.{EtcHandshaker, EtcHandshakerConfiguration, Handshaker}
import io.iohk.ethereum.network.p2p.EthereumMessageDecoder
import io.iohk.ethereum.network.rlpx.RLPxConnectionHandler.RLPxConfiguration
import io.iohk.ethereum.network.{ForkResolver, PeerEventBusActor, PeerManagerActor}
import io.iohk.ethereum.nodebuilder.{AuthHandshakerBuilder, NodeKeyBuilder}
import io.iohk.ethereum.security.SecureRandomBuilder
import io.iohk.ethereum.utils.{Config, NodeStatus, ServerStatus}
import monix.reactive.Observable
import org.bouncycastle.util.encoders.Hex

import scala.concurrent.duration._

object DumpChainApp extends App with NodeKeyBuilder with SecureRandomBuilder with AuthHandshakerBuilder {
  val conf = ConfigFactory.load("txExecTest/chainDump.conf")
  val node = conf.getString("node")
  val genesisHash = ByteString(Hex.decode(conf.getString("genesisHash")))
  val privateNetworkId = conf.getInt("networkId")
  val startBlock = conf.getInt("startBlock")
  val maxBlocks = conf.getInt("maxBlocks")

  val blockchainConfig = Config.blockchains.blockchainConfig
  val discoveryConfig = DiscoveryConfig(Config.config, blockchainConfig.bootstrapNodes)

  val peerConfig = new PeerConfiguration {
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

  val actorSystem = ActorSystem("mantis_system")
  trait PruningConfig extends PruningModeComponent {
    override val pruningMode: PruningMode = ArchivePruning
  }
  val storagesInstance = new RocksDbDataSourceComponent with PruningConfig with Storages.DefaultStorages

  val blockchain: Blockchain = new BlockchainMock(genesisHash)

  val nodeStatus =
    NodeStatus(key = nodeKey, serverStatus = ServerStatus.NotListening, discoveryStatus = ServerStatus.NotListening)

  lazy val nodeStatusHolder = new AtomicReference(nodeStatus)

  lazy val forkResolverOpt = blockchainConfig.daoForkConfig.map(new ForkResolver.EtcForkResolver(_))

  private val handshakerConfiguration: EtcHandshakerConfiguration =
    new EtcHandshakerConfiguration {
      override val forkResolverOpt: Option[ForkResolver] = DumpChainApp.forkResolverOpt
      override val nodeStatusHolder: AtomicReference[NodeStatus] = DumpChainApp.nodeStatusHolder
      override val peerConfiguration: PeerConfiguration = peerConfig
      override val blockchain: Blockchain = DumpChainApp.blockchain
      override val appStateStorage: AppStateStorage = storagesInstance.storages.appStateStorage
      override val protocolVersion: Int = Config.Network.protocolVersion
    }

  lazy val handshaker: Handshaker[PeerInfo] = EtcHandshaker(handshakerConfiguration)

  val peerMessageBus = actorSystem.actorOf(PeerEventBusActor.props)

  val peerStatistics = actorSystem.actorOf(PeerStatisticsActor.props(peerMessageBus, 1.minute, 30)(Clock.systemUTC()))

  val blacklist: CacheBasedBlacklist = CacheBasedBlacklist.empty(100)

  val peerManager = actorSystem.actorOf(
    PeerManagerActor.props(
      peerDiscoveryManager = actorSystem.deadLetters, // TODO: fixme
      peerConfiguration = peerConfig,
      peerMessageBus = peerMessageBus,
      peerStatistics = peerStatistics,
      knownNodesManager = actorSystem.deadLetters, // TODO: fixme
      handshaker = handshaker,
      authHandshaker = authHandshaker,
      messageDecoder = EthereumMessageDecoder,
      discoveryConfig = discoveryConfig,
      blacklist = blacklist,
      bestProtocolVersion = Config.Network.protocolVersion
    ),
    "peer-manager"
  )
  peerManager ! PeerManagerActor.StartConnecting

  actorSystem.actorOf(DumpChainActor.props(peerManager, peerMessageBus, startBlock, maxBlocks, node), "dumper")
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

  override protected def getHashByBlockNumber(number: BigInt): Option[ByteString] = Some(genesisHash)

  override def getBlockHeaderByHash(hash: ByteString): Option[BlockHeader] = Some(new FakeHeader())

  override def getBlockBodyByHash(hash: ByteString): Option[BlockBody] = ???

  override def getMptNodeByHash(hash: ByteString): Option[MptNode] = ???

  override def storeBlockHeader(blockHeader: BlockHeader): DataSourceBatchUpdate = ???

  override def storeBlockBody(blockHash: ByteString, blockBody: BlockBody): DataSourceBatchUpdate = ???

  override def storeReceipts(blockHash: ByteString, receipts: Seq[Receipt]): DataSourceBatchUpdate = ???

  override def storeEvmCode(hash: ByteString, evmCode: ByteString): DataSourceBatchUpdate = ???

  override def storeChainWeight(blockhash: ByteString, chainWeight: ChainWeight): DataSourceBatchUpdate = ???

  override def saveNode(nodeHash: NodeHash, nodeEncoded: NodeEncoded, blockNumber: BigInt): Unit = ???

  override def removeBlock(hash: ByteString, withState: Boolean = true): Unit = ???

  override def getChainWeightByHash(blockhash: ByteString): Option[ChainWeight] = ???

  override def getEvmCodeByHash(hash: ByteString): Option[ByteString] = ???

  override def getReceiptsByHash(blockhash: ByteString): Option[Seq[Receipt]] = ???

  def getAccount(address: Address, blockNumber: BigInt): Option[Account] = ???

  override def getAccountStorageAt(rootHash: ByteString, position: BigInt, ethCompatibleStorage: Boolean): ByteString =
    ???

  override def getTransactionLocation(txHash: ByteString): Option[TransactionLocation] = ???

  override type S = InMemoryWorldStateProxyStorage
  override type WS = InMemoryWorldStateProxy

  override def getWorldStateProxy(
      blockNumber: BigInt,
      accountStartNonce: UInt256,
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): InMemoryWorldStateProxy = ???

  override def getReadOnlyWorldStateProxy(
      blockNumber: Option[BigInt],
      accountStartNonce: UInt256,
      stateRootHash: ByteString,
      noEmptyAccounts: Boolean,
      ethCompatibleStorage: Boolean
  ): InMemoryWorldStateProxy = ???

  def getBestBlockNumber(): BigInt = ???

  def saveBlockNumber(number: BigInt, hash: NodeHash): Unit = ???

  def saveBestKnownBlocks(bestBlockNumber: BigInt, latestCheckpointNumber: Option[BigInt] = None): Unit = ???

  def getBestBlock(): Option[Block] = ???

  override def save(block: Block, receipts: Seq[Receipt], weight: ChainWeight, saveAsBestBlock: Boolean): Unit = ???

  override def getStateStorage: StateStorage = ???

  override def getLatestCheckpointBlockNumber(): BigInt = ???

  override def mptStateSavedKeys(): Observable[Either[RocksDbDataSource.IterationError, NodeHash]] = ???
}
