package io.iohk.ethereum.consensus
package atomixraft

import io.atomix.cluster._
import io.atomix.cluster.impl.{DefaultClusterMetadataService, DefaultClusterService}
import io.atomix.cluster.messaging.impl.DefaultClusterMessagingService
import io.atomix.core.election.LeaderElectionType
import io.atomix.messaging.ManagedMessagingService
import io.atomix.messaging.impl.NettyMessagingService
import io.atomix.protocols.raft.RaftServer
import io.atomix.protocols.raft.partition.impl.{RaftNamespaces, RaftServerCommunicator}
import io.atomix.protocols.raft.protocol.{TransferRequest, TransferResponse}
import io.atomix.protocols.raft.storage.RaftStorage
import io.atomix.utils.concurrent.ThreadModel
import io.atomix.utils.serializer.{KryoNamespace, Serializer}
import io.iohk.ethereum.consensus.atomixraft.AtomixRaftMiner.{IAmTheLeader, Init}
import io.iohk.ethereum.consensus.atomixraft.blocks.AtomixRaftBlockGenerator
import io.iohk.ethereum.consensus.blocks.BlockGenerator
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.consensus.validators.std.StdValidators
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.vm.VM

class AtomixRaftConsensus private(
  vm: VM,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  fullConsensusConfig: FullConsensusConfig[AtomixRaftConfig],
  _validators: Validators,
  _blockGenerator: AtomixRaftBlockGenerator
) extends ConsensusImpl[AtomixRaftConfig](
  vm,
  blockchain,
  blockchainConfig,
  fullConsensusConfig
) {

  private[this] final val miner = new AtomixRaftMinerRef

  private[this] final val raftServer = new RaftServerRef

  private[this] val raftConfig: AtomixRaftConfig = config.specific

  private[this] def setupMiner(node: Node): Unit = {
    miner.setOnce {
      val miner = AtomixRaftMiner(node)
      miner ! Init
      miner
    }
  }

  private[this] def onRaftServerStarted(messagingService: ManagedMessagingService): Unit = {
    log.info("Raft server started at " + messagingService.endpoint)
  }

  private[this] def onLeader(): Unit = {
    miner ! IAmTheLeader
  }

  private[this] def onClusterEvent(event: ClusterEvent): Unit = {
    log.info("***** " + event)
  }

  private[this] def onRoleChange(role: RaftServer.Role): Unit = {
    log.info("***** Role changed to " + role)
    if(role == RaftServer.Role.LEADER) {
      onLeader()
    }
  }

  private[this] def addListeners(clusterService: DefaultClusterService, server: RaftServer): Unit = {
    server.addRoleChangeListener(onRoleChange)
    clusterService.addListener(onClusterEvent)
  }

  private[atomixraft] final val RAFT_PROTOCOL_EX = KryoNamespace.builder()
    .register(RaftNamespaces.RAFT_PROTOCOL)
    .register(classOf[TransferRequest]) // Nice bug in the lib, this was missing!
    .register(classOf[TransferResponse]) // Nice bug in the lib, this was missing!
    .build()

  private[this] def setupRaftServer(node: Node): Unit = {
    raftServer.setOnce {
      import raftConfig.{bootstrapNodes, dataDir, localNode}

      import scala.collection.JavaConverters._

      val messagingService = NettyMessagingService.builder.withEndpoint(localNode.endpoint).build

      val metadata = ClusterMetadata.builder.withBootstrapNodes(bootstrapNodes.asJava).build
      val metadataService = new DefaultClusterMetadataService(metadata, messagingService)
      val clusterService = new DefaultClusterService(localNode, metadataService, messagingService)
      val clusterMessagingService = new DefaultClusterMessagingService(clusterService, messagingService)

      val protocol = new RaftServerCommunicator(Serializer.using(RAFT_PROTOCOL_EX), clusterMessagingService)
      val raftStorage = RaftStorage.builder
        .withSerializer(Serializer.using(RaftNamespaces.RAFT_STORAGE))
        .withDirectory(dataDir)
        .build

      val server = RaftServer.builder(localNode.id)
        .withThreadModel(ThreadModel.SHARED_THREAD_POOL)
        .withClusterService(clusterService)
        .withProtocol(protocol)
        .withStorage(raftStorage)
        /*.withElectionTimeout(Duration.ofMillis(3000))   // FIXME configurable
        .withHeartbeatInterval(Duration.ofMillis(300))*/  // FIXME configurable
        .addPrimitiveType(LeaderElectionType.instance[Any])
        .build

      addListeners(clusterService, server)

      val clusterNodeIds = bootstrapNodes.map(_.id())
      messagingService.start
        .thenComposeAsync(_ ⇒ metadataService.start)
        .thenComposeAsync(_ ⇒ clusterService.start)
        .thenComposeAsync(_ ⇒ clusterMessagingService.start)
        .thenComposeAsync(_ ⇒ server.bootstrap(clusterNodeIds.asJava))
        .thenRun(() ⇒ onRaftServerStarted(messagingService))

      server
    }
  }

  def isLeader: Option[Boolean] = raftServer.map(_.isLeader) // None means we do not know

  def promoteToLeader(): Unit = raftServer.map(_.promote().join())

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit = {
    setupMiner(node)
    setupRaftServer(node)
  }

  def stopProtocol(): Unit = {
    miner.kill()
    raftServer.stop()
  }

  def protocol: Protocol = Protocol.AtomixRaft

  /**
   * Provides the set of validators specific to this consensus protocol.
   */
  def validators: Validators = this._validators


  /** Internal API, used for testing */
  protected def newBlockGenerator(validators: Validators): AtomixRaftBlockGenerator = {
    val blockPreparator = new BlockPreparator(
      vm = vm,
      signedTxValidator = validators.signedTransactionValidator,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig
    )

    new AtomixRaftBlockGenerator(
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      consensusConfig = config.generic,
      blockPreparator = blockPreparator,
      blockTimestampProvider = blockGenerator.blockTimestampProvider
    )
  }

  /** Internal API, used for testing */
  def withValidators(validators: Validators): AtomixRaftConsensus = {
    val blockGenerator = newBlockGenerator(validators)

    new AtomixRaftConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      fullConsensusConfig = fullConsensusConfig,
      _validators = validators,
      _blockGenerator = blockGenerator
    )
  }

  /** Internal API, used for testing */
  def withVM(vm: VM): AtomixRaftConsensus =
    new AtomixRaftConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      fullConsensusConfig = fullConsensusConfig,
      _validators = _validators,
      _blockGenerator = blockGenerator
    )


  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: BlockGenerator): AtomixRaftConsensus =
    new AtomixRaftConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      fullConsensusConfig = fullConsensusConfig,
      _validators = validators,
      _blockGenerator = blockGenerator.asInstanceOf[AtomixRaftBlockGenerator]
    )

  /**
   * Returns the [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator]]
   * this consensus protocol uses.
   */
  def blockGenerator: AtomixRaftBlockGenerator = this._blockGenerator
}

object AtomixRaftConsensus {
  def apply(
    vm: VM,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    fullConsensusConfig: FullConsensusConfig[AtomixRaftConfig]
  ): AtomixRaftConsensus = {

    val validators = StdValidators(blockchainConfig)

    val blockPreparator = new BlockPreparator(
      vm = vm,
      signedTxValidator = validators.signedTransactionValidator,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig
    )

    val blockGenerator = new AtomixRaftBlockGenerator(
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      consensusConfig = fullConsensusConfig.generic,
      blockPreparator = blockPreparator
    )

    new AtomixRaftConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      fullConsensusConfig = fullConsensusConfig,
      _validators = validators,
      _blockGenerator = blockGenerator
    )
  }
}
