package io.iohk.ethereum.sync.util

import akka.actor.ActorRef
import akka.actor.typed
import akka.actor.typed.scaladsl.adapter._
import akka.util.ByteString

import cats.effect.Resource

import monix.eval.Task
import monix.execution.Scheduler

import scala.concurrent.duration._

import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.PeersClient
import io.iohk.ethereum.blockchain.sync.SyncProtocol
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcast.BlockToBroadcast
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlock
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher
import io.iohk.ethereum.blockchain.sync.regular.BlockFetcher.AdaptedMessageFromEventBus
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter
import io.iohk.ethereum.blockchain.sync.regular.BlockImporter.Start
import io.iohk.ethereum.blockchain.sync.regular.RegularSync
import io.iohk.ethereum.blockchain.sync.regular.RegularSync.NewCheckpoint
import io.iohk.ethereum.checkpointing.CheckpointingTestHelpers
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.mining.FullMiningConfig
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.consensus.mining.Protocol.NoAdditionalPoWData
import io.iohk.ethereum.consensus.pow
import io.iohk.ethereum.consensus.pow.EthashConfig
import io.iohk.ethereum.consensus.pow.PoWMining
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.crypto
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.mpt.MerklePatriciaTrie
import io.iohk.ethereum.network.PeerId
import io.iohk.ethereum.network.p2p.messages.BaseETH6XMessages.NewBlock
import io.iohk.ethereum.nodebuilder.VmSetup
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils.FakePeerCustomConfig.defaultConfig
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils._
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils._

object RegularSyncItSpecUtils {

  class ValidatorsExecutorAlwaysSucceed extends MockValidatorsAlwaysSucceed {
    override def validateBlockAfterExecution(
        block: Block,
        stateRootHash: ByteString,
        receipts: Seq[Receipt],
        gasUsed: BigInt
    ): Either[BlockExecutionError, BlockExecutionSuccess] = Right(BlockExecutionSuccess)
  }

  object ValidatorsExecutorAlwaysSucceed extends ValidatorsExecutorAlwaysSucceed

  class FakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig)
      extends CommonFakePeer(peerName, fakePeerCustomConfig) {

    def buildEthashMining(): pow.PoWMining = {
      val miningConfig: MiningConfig = MiningConfig(Config.config)
      val specificConfig: EthashConfig = pow.EthashConfig(config)
      val fullConfig = FullMiningConfig(miningConfig, specificConfig)
      val vm = VmSetup.vm(VmConfig(config), blockchainConfig, testMode = false)
      val mining =
        PoWMining(
          vm,
          storagesInstance.storages.evmCodeStorage,
          bl,
          blockchainReader,
          blockchainConfig,
          fullConfig,
          ValidatorsExecutorAlwaysSucceed,
          NoAdditionalPoWData
        )
      mining
    }

    lazy val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator
    lazy val peersClient: ActorRef =
      system.actorOf(
        PeersClient.props(etcPeerManager, peerEventBus, blacklist, testSyncConfig, system.scheduler),
        "peers-client"
      )

    lazy val mining: PoWMining = buildEthashMining()

    lazy val blockQueue: BlockQueue = BlockQueue(bl, blockchainReader, syncConfig)
    lazy val blockValidation = new BlockValidation(mining, blockchainReader, blockQueue)
    lazy val blockExecution =
      new BlockExecution(
        bl,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.evmCodeStorage,
        blockchainConfig,
        mining.blockPreparator,
        blockValidation
      )
    lazy val blockImport: BlockImport =
      new BlockImport(
        bl,
        blockchainReader,
        blockchainWriter,
        blockQueue,
        blockValidation,
        blockExecution,
        Scheduler.global
      )

    lazy val ommersPool: ActorRef = system.actorOf(OmmersPool.props(blockchainReader, 1), "ommers-pool")

    lazy val pendingTransactionsManager: ActorRef = system.actorOf(
      PendingTransactionsManager.props(TxPoolConfig(config), peerManager, etcPeerManager, peerEventBus),
      "pending-transactions-manager"
    )

    lazy val validators: ValidatorsExecutor = buildEthashMining().validators

    val broadcasterRef: ActorRef = system.actorOf(
      BlockBroadcasterActor
        .props(
          new BlockBroadcast(etcPeerManager),
          peerEventBus,
          etcPeerManager,
          blacklist,
          syncConfig,
          system.scheduler
        ),
      "block-broadcaster"
    )

    val fetcher: typed.ActorRef[BlockFetcher.FetchCommand] =
      system.spawn(
        BlockFetcher(peersClient, peerEventBus, regularSync, syncConfig, validators.blockValidator),
        "block-fetcher"
      )

    lazy val blockImporter: ActorRef = system.actorOf(
      BlockImporter.props(
        fetcher.toClassic,
        blockImport,
        bl,
        blockchainReader,
        storagesInstance.storages.stateStorage,
        new BranchResolution(bl, blockchainReader),
        syncConfig,
        ommersPool,
        broadcasterRef,
        pendingTransactionsManager,
        regularSync
      )
    )

    lazy val regularSync: ActorRef = system.actorOf(
      RegularSync.props(
        peersClient,
        etcPeerManager,
        peerEventBus,
        blockImport,
        bl,
        blockchainReader,
        storagesInstance.storages.stateStorage,
        new BranchResolution(bl, blockchainReader),
        validators.blockValidator,
        blacklist,
        testSyncConfig,
        ommersPool,
        pendingTransactionsManager,
        system.scheduler
      )
    )

    def startRegularSync(): Task[Unit] = Task {
      regularSync ! SyncProtocol.Start
    }

    def broadcastBlock(
        blockNumber: Option[Int] = None
    )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] =
      Task(blockNumber match {
        case Some(bNumber) =>
          blockchainReader
            .getBlockByNumber(bNumber)
            .getOrElse(throw new RuntimeException(s"block by number: $bNumber doesn't exist"))
        case None => blockchainReader.getBestBlock().get
      }).flatMap { block =>
        Task {
          val currentWeight = bl
            .getChainWeightByHash(block.hash)
            .getOrElse(throw new RuntimeException(s"ChainWeight by hash: ${block.hash} doesn't exist"))
          val currentWorld = getMptForBlock(block)
          val (newBlock, newWeight, _) = createChildBlock(block, currentWeight, currentWorld)(updateWorldForBlock)
          broadcastBlock(newBlock, newWeight)
        }
      }

    def waitForRegularSyncLoadLastBlock(blockNumber: BigInt): Task[Boolean] =
      retryUntilWithDelay(Task(blockchainReader.getBestBlockNumber() == blockNumber), 1.second, 90)(isDone => isDone)

    def mineNewBlock(
        plusDifficulty: BigInt = 0
    )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] = Task {
      val block: Block = blockchainReader.getBestBlock().get
      val currentWeight = bl
        .getChainWeightByHash(block.hash)
        .getOrElse(throw new RuntimeException(s"ChainWeight by hash: ${block.hash} doesn't exist"))
      val currentWorld = getMptForBlock(block)
      val (newBlock, _, _) =
        createChildBlock(block, currentWeight, currentWorld, plusDifficulty)(updateWorldForBlock)
      regularSync ! SyncProtocol.MinedBlock(newBlock)
    }

    def mineNewBlocks(delay: FiniteDuration, nBlocks: Int)(
        updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy
    ): Task[Unit] =
      if (nBlocks > 0) {
        mineNewBlock()(updateWorldForBlock)
          .delayExecution(delay)
          .flatMap(_ => mineNewBlocks(delay, nBlocks - 1)(updateWorldForBlock))
      } else Task(())

    def addCheckpointedBlock(parent: Block): Task[Unit] = Task {
      val signatures = CheckpointingTestHelpers.createCheckpointSignatures(
        Seq(crypto.generateKeyPair(secureRandom)),
        parent.hash
      )
      val checkpoint = checkpointBlockGenerator.generate(parent, Checkpoint(signatures))
      regularSync ! NewCheckpoint(checkpoint)
    }

    def getCheckpointFromPeer(checkpoint: Block, peerId: PeerId): Task[Unit] = Task {
      blockImporter ! Start
      fetcher ! AdaptedMessageFromEventBus(NewBlock(checkpoint, checkpoint.header.difficulty), peerId)
    }

    private def getMptForBlock(block: Block) =
      InMemoryWorldStateProxy(
        storagesInstance.storages.evmCodeStorage,
        bl.getBackingMptStorage(block.number),
        (number: BigInt) => blockchainReader.getBlockHeaderByNumber(number).map(_.hash),
        UInt256.Zero,
        ByteString(MerklePatriciaTrie.EmptyRootHash),
        noEmptyAccounts = false,
        ethCompatibleStorage = true
      )

    private def broadcastBlock(block: Block, weight: ChainWeight) =
      broadcasterActor ! BroadcastBlock(BlockToBroadcast(block, weight))

    private def createChildBlock(
        parent: Block,
        parentWeight: ChainWeight,
        parentWorld: InMemoryWorldStateProxy,
        plusDifficulty: BigInt = 0
    )(
        updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy
    ): (Block, ChainWeight, InMemoryWorldStateProxy) = {
      val newBlockNumber = parent.header.number + 1
      val newWorld = updateWorldForBlock(newBlockNumber, parentWorld)
      val newBlock = parent.copy(header =
        parent.header.copy(
          parentHash = parent.header.hash,
          number = newBlockNumber,
          stateRoot = newWorld.stateRootHash,
          difficulty = plusDifficulty + parent.header.difficulty
        )
      )
      val newWeight = parentWeight.increase(newBlock.header)
      (newBlock, newWeight, parentWorld)
    }
  }

  object FakePeer {

    def startFakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig): Task[FakePeer] =
      for {
        peer <- Task(new FakePeer(peerName, fakePeerCustomConfig))
        _ <- peer.startPeer()
      } yield peer

    def start1FakePeerRes(
        fakePeerCustomConfig: FakePeerCustomConfig = defaultConfig,
        name: String
    ): Resource[Task, FakePeer] =
      Resource.make {
        startFakePeer(name, fakePeerCustomConfig)
      } { peer =>
        peer.shutdown()
      }

    def start2FakePeersRes(
        fakePeerCustomConfig1: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig2: FakePeerCustomConfig = defaultConfig
    ): Resource[Task, (FakePeer, FakePeer)] =
      for {
        peer1 <- start1FakePeerRes(fakePeerCustomConfig1, "Peer1")
        peer2 <- start1FakePeerRes(fakePeerCustomConfig2, "Peer2")
      } yield (peer1, peer2)

  }
}
