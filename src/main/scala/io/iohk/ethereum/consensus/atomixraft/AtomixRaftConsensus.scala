package io.iohk.ethereum.consensus
package atomixraft

import java.time.{Duration => JDuration}

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
import io.iohk.ethereum.utils.{BlockchainConfig, Logger, VmConfig, Riemann}

class AtomixRaftConsensus private(
  val vm: VMImpl,
  blockchain: BlockchainImpl,
  blockchainConfig: BlockchainConfig,
  val config: FullConsensusConfig[AtomixRaftConfig],
  val validators: Validators,
  val blockGenerator: AtomixRaftBlockGenerator
) extends TestConsensus with Logger {

  type Config = AtomixRaftConfig

  private[this] final val forgerRef = new AtomixRaftForgerRef

  private[this] final val raftServer = new RaftServerRef

  private[this] final val raftConfig: AtomixRaftConfig = config.specific

  private[this] final val _blockPreparator = new BlockPreparator(
    vm = vm,
    signedTxValidator = validators.signedTransactionValidator,
    blockchain = blockchain,
    blockchainConfig = blockchainConfig
  )

  private[this] final val metrics = new AtomixRaftConsensusMetrics(Metrics.get())

  private[this] def startForger(node: Node): Unit = {
    forgerRef.setOnce {
      val miner = AtomixRaftForger(node)
      miner ! Init
      miner
    }
  }

  private[this] def stopForger(): Unit = forgerRef.kill()

  private[this] def onRaftServerStarted(messagingService: ManagedMessagingService): Unit = {
    Riemann.ok("raft server started").attribute("endpoint", messagingService.endpoint.toString).send
  }

  private[this] def onLeaderWithMiningEnabled(): Unit = {
    metrics.LeaderEvent.trigger()
    metrics.BecomeLeaderCounter.increment()

    forgerRef ! IAmTheLeader
  }

  private[this] def onLeaderWithMiningDisabled(): Unit = {
    Riemann.warning("raft server elected").attribute(ConsensusConfig.Keys.MiningEnabled, config.miningEnabled.toString).send

    raftServer.run { server ⇒
      val cluster = server.cluster()
      val leader = cluster.getLeader

      // We have just become the leader but what is the probability of having a re-election and a new leader,
      // thus kicking out a different leader?
      Riemann.ok("raft server demoting").attribute("leader", leader.toString).send
      try leader.demote().join()
      catch {
        case t: Throwable ⇒
          Riemann.exception("raft server demoting", t).attribute("leader", leader.toString).send
          throw t
      }
    }
  }

  private[this] def onClusterEvent(event: ClusterEvent): Unit = {
    Riemann.ok("atomix server cluster event").attribute("event", event.toString).send
  }

  private[this] def onRoleChange(role: RaftServer.Role): Unit = {
    Riemann.ok("raft server role changed").attribute("role", role.toString).attribute(ConsensusConfig.Keys.MiningEnabled, config.miningEnabled.toString).send

    if(role == RaftServer.Role.LEADER) {
      if(config.miningEnabled) {
        onLeaderWithMiningEnabled()
      }
      else {
        onLeaderWithMiningDisabled()
      }
    }

    metrics.ChangeRoleCounter.increment()
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

  private[this] def startRaftServer(node: Node): Unit = {
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

  private[this] def stopRaftServer(): Unit = raftServer.stop()

  def isLeader: Option[Boolean] = raftServer.run(_.isLeader) // None means we do not know

  /**
   * This is used by the [[io.iohk.ethereum.consensus.Consensus#blockGenerator blockGenerator]].
   */
  def blockPreparator: BlockPreparator = this._blockPreparator

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit = {
    if(config.miningEnabled) {
      // The current design assumes all nodes participate in forging, so not forging
      // is considered to be a temporary solution for operational reasons.
      startForger(node)
    }

    // We need to allow this even if not forging, since in the current design the configuration
    // presents all nodes as possible Raft leaders.
    // BUT we need to take care when the protocol elects us as leader, to relinquish control
    // and let it re-elect some other node.
    // NOTE Remember that the current design is: Leader-is-Forger.
    startRaftServer(node)
  }

  def stopProtocol(): Unit = {
    if(config.miningEnabled) {
      stopForger()
    }

    stopRaftServer()
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
    config: FullConsensusConfig[AtomixRaftConfig],
    vmConfig: VmConfig
  ): AtomixRaftConsensus = {

    val validators = AtomixRaftValidators(blockchainConfig, vmConfig)

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
