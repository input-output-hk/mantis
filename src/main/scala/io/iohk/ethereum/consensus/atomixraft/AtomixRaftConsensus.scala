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
import io.iohk.ethereum.consensus.atomixraft.Miner.{IAmTheLeader, Init}
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.utils.{BlockchainConfig, Logger}
import io.iohk.ethereum.validators.{BlockHeaderValidator, BlockHeaderValidatorImpl}

class AtomixRaftConsensus(
  blockchainConfig: BlockchainConfig,
  consensusConfig: ConsensusConfig,
  raftConfig: AtomixRaftConfig
) extends Consensus with Logger {

  private[this] final val defaultValidator = new BlockHeaderValidatorImpl(blockchainConfig)

  private[this] final val miner = new MinerRef

  private[this] final val raftServer = new RaftServerRef

  private[this] def setupMiner(node: Node): Unit = {
    miner.setOnce {
      val miner = Miner(node, raftConfig)
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
        /*.withElectionTimeout(Duration.ofMillis(3000))
        .withHeartbeatInterval(Duration.ofMillis(300))*/
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

  /**
   * Provides the [[io.iohk.ethereum.validators.BlockHeaderValidator BlockHeaderValidator]] that is specific
   * to this consensus protocol.
   *
   * The returned validator does whatever Ethereum requires except from PoW-related validation,
   * since there is no PoW in this consensus protocol.
   */
  def blockHeaderValidator: BlockHeaderValidator = defaultValidator
}
