package io.iohk.ethereum.consensus
package atomixraft

import java.time.{Duration ⇒ JDuration}

import com.google.common.collect.Maps
import io.atomix.cluster._
import io.atomix.cluster.impl.{DefaultBootstrapMetadataService, DefaultClusterMetadataService, DefaultClusterService, DefaultCoreMetadataService}
import io.atomix.cluster.messaging.impl.DefaultClusterMessagingService
import io.atomix.core.election.LeaderElectionType
import io.atomix.cluster.messaging.ManagedMessagingService
import io.atomix.cluster.messaging.impl.{NettyBroadcastService, NettyMessagingService}
import io.atomix.core.Atomix
import io.atomix.primitive.{PrimitiveType, PrimitiveTypeRegistry}
import io.atomix.primitive.impl.DefaultPrimitiveTypeRegistry
import io.atomix.protocols.raft.RaftServer
import io.atomix.protocols.raft.partition.impl.{RaftNamespaces, RaftServerCommunicator}
import io.atomix.protocols.raft.protocol.{TransferRequest, TransferResponse}
import io.atomix.protocols.raft.storage.RaftStorage
import io.atomix.utils.concurrent.ThreadModel
import io.atomix.utils.serializer.{Serializer, Namespace ⇒ KryoNamespace}
import io.iohk.ethereum.consensus.atomixraft.AtomixRaftForger.{IAmTheLeader, Init}
import io.iohk.ethereum.consensus.atomixraft.blocks.AtomixRaftBlockGenerator
import io.iohk.ethereum.consensus.atomixraft.validators.AtomixRaftValidators
import io.iohk.ethereum.consensus.blocks.TestBlockGenerator
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.metrics.Metrics
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}

class AtomixRaftConsensus private(
  val vm: VMImpl,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  val config: FullConsensusConfig[AtomixRaftConfig],
  val validators: Validators,
  val blockGenerator: AtomixRaftBlockGenerator
) extends TestConsensus with Logger {

  type Config = AtomixRaftConfig

  private[this] final val miner = new AtomixRaftForgerRef

  private[this] final val raftServer = new RaftServerRef

  private[this] final val raftConfig: AtomixRaftConfig = config.specific

  private[this] final val _blockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig
  )

  private[this] final val metrics = new AtomixRaftConsensusMetrics(Metrics.get())

  private[this] def setupMiner(node: Node): Unit = {
    miner.setOnce {
      val miner = AtomixRaftForger(node)
      miner ! Init
      miner
    }
  }

  private[this] def onRaftServerStarted(messagingService: ManagedMessagingService): Unit = {
    log.info("Raft server started at " + messagingService.address())
  }

  private[this] def onLeader(): Unit = {
    metrics.LeaderEvent.trigger()
    metrics.BecomeLeaderCounter.increment()

    miner ! IAmTheLeader
  }

  private[this] def onClusterMembershipEvent(event: ClusterMembershipEvent): Unit = {
    log.info("***** " + event)
  }

  private[this] def onRoleChange(role: RaftServer.Role): Unit = {
    log.info("***** Role changed to " + role)
    if(role == RaftServer.Role.LEADER) {
      onLeader()
    }

    metrics.ChangeRoleCounter.increment()
  }

  private[this] def addListeners(clusterMembership: ClusterMembershipService, server: RaftServer): Unit = {
    server.addRoleChangeListener(onRoleChange)
    clusterMembership.addListener(onClusterMembershipEvent)
  }

  private[atomixraft] final val RAFT_PROTOCOL_EX = KryoNamespace.builder()
    .register(RaftNamespaces.RAFT_PROTOCOL)
    .register(classOf[TransferRequest]) // Nice bug in the lib, this was missing!
    .register(classOf[TransferResponse]) // Nice bug in the lib, this was missing!
    .build()


  private[this] def setupRaftCluster(node: Node): Unit = {
    import raftConfig.{bootstrapNodes, dataDir, localNode}

    import scala.collection.JavaConverters._

    val cluster = AtomixCluster.builder()
      .withLocalMember(localNode)
      .withMembers(bootstrapNodes.asJava)
      .build()

    cluster.start()

    val raftStorage = RaftStorage.builder()
      .withSerializer(Serializer.using(RaftNamespaces.RAFT_STORAGE))
      .withDirectory(dataDir)
      .build

    val clusterCommunication = cluster.getCommunicationService
    val clusterMembership = cluster.getMembershipService

    val protocol = new RaftServerCommunicator(Serializer.using(RAFT_PROTOCOL_EX), clusterCommunication)

    val primitiveTypes = Maps.newHashMap[String, PrimitiveType]()
    val primitiveTypeRegistry: PrimitiveTypeRegistry = new DefaultPrimitiveTypeRegistry()

    val server = RaftServer.builder(localNode.id)
      .withMembershipService(clusterMembership)
      .withThreadModel(ThreadModel.SHARED_THREAD_POOL)
      .withProtocol(protocol)
      .withStorage(raftStorage)
      .withElectionTimeout(JDuration.ofMillis(config.specific.electionTimeout.toMillis))
      .withHeartbeatInterval(JDuration.ofMillis(config.specific.heartbeatInterval.toMillis))
      .build

    addListeners(clusterMembership, server)

  }

  private[this] def setupRaftServer(node: Node): Unit = {
    raftServer.setOnce {
      import raftConfig.{bootstrapNodes, dataDir, localNode}

      import scala.collection.JavaConverters._

      val messagingService = NettyMessagingService.builder.withAddress(localNode.address()).build

      val metadata = ClusterMetadata.builder.withNodes(bootstrapNodes.asJava).build
      val bootstrapMetadataService = new DefaultBootstrapMetadataService(metadata)
      val coreMetadataService = new DefaultCoreMetadataService(metadata, messagingService)
      val broadcastService = new NettyBroadcastService(localNode.address(), , true)

      val clusterService = new DefaultClusterService(localNode, bootstrapMetadataService, coreMetadataService, messagingService)
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
        .withElectionTimeout(JDuration.ofMillis(config.specific.electionTimeout.toMillis))
        .withHeartbeatInterval(JDuration.ofMillis(config.specific.heartbeatInterval.toMillis))
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

  def isLeader: Option[Boolean] = raftServer.run(_.isLeader) // None means we do not know

  def promoteToLeader(): Unit = raftServer.run(_.promote().join())

  /**
   * This is used by the [[io.iohk.ethereum.consensus.Consensus#blockGenerator blockGenerator]].
   */
  def blockPreparator: BlockPreparator = this._blockPreparator

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
      config = config,
      validators = validators,
      blockGenerator = blockGenerator
    )
  }

  /** Internal API, used for testing */
  def withVM(vm: VMImpl): AtomixRaftConsensus =
    new AtomixRaftConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator
    )


  /** Internal API, used for testing */
  def withBlockGenerator(blockGenerator: TestBlockGenerator): AtomixRaftConsensus =
    new AtomixRaftConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator.asInstanceOf[AtomixRaftBlockGenerator]
    )
}

object AtomixRaftConsensus {
  def apply(
    vm: VMImpl,
    blockchain: BlockchainImpl,
    blockchainConfig: BlockchainConfig,
    config: FullConsensusConfig[AtomixRaftConfig]
  ): AtomixRaftConsensus = {

    val validators = AtomixRaftValidators(blockchainConfig)

    val blockPreparator = new BlockPreparator(
      vm = vm,
      signedTxValidator = validators.signedTransactionValidator,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig
    )

    val blockGenerator = new AtomixRaftBlockGenerator(
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      consensusConfig = config.generic,
      blockPreparator = blockPreparator
    )

    new AtomixRaftConsensus(
      vm = vm,
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      config = config,
      validators = validators,
      blockGenerator = blockGenerator
    )
  }
}
