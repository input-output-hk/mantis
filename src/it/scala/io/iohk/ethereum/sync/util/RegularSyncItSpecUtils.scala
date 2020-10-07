package io.iohk.ethereum.sync.util

import akka.actor.ActorRef
import akka.util.ByteString
import cats.effect.Resource
import io.iohk.ethereum.Mocks.MockValidatorsAlwaysSucceed
import io.iohk.ethereum.blockchain.sync.{PeersClient, SyncProtocol}
import io.iohk.ethereum.blockchain.sync.regular.BlockBroadcasterActor.BroadcastBlock
import io.iohk.ethereum.blockchain.sync.regular.RegularSync
import io.iohk.ethereum.consensus.blocks.CheckpointBlockGenerator
import io.iohk.ethereum.consensus.ethash.{EthashConfig, EthashConsensus}
import io.iohk.ethereum.consensus.{ConsensusConfig, FullConsensusConfig, ethash}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.network.p2p.messages.CommonMessages.NewBlock
import io.iohk.ethereum.nodebuilder.VmSetup
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils.FakePeerCustomConfig.defaultConfig
import io.iohk.ethereum.sync.util.SyncCommonItSpecUtils._
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils._
import io.iohk.ethereum.vm.EvmConfig
import monix.eval.Task

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
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

    def buildEthashConsensus(): ethash.EthashConsensus = {
      val consensusConfig: ConsensusConfig = ConsensusConfig(Config.config)
      val specificConfig: EthashConfig = ethash.EthashConfig(config)
      val fullConfig = FullConsensusConfig(consensusConfig, specificConfig)
      val vm = VmSetup.vm(VmConfig(config), blockchainConfig, testMode = false)
      val consensus = EthashConsensus(vm, bl, blockchainConfig, fullConfig, ValidatorsExecutorAlwaysSucceed)
      consensus
    }

    lazy val checkpointBlockGenerator: CheckpointBlockGenerator = new CheckpointBlockGenerator
    lazy val peersClient: ActorRef =
      system.actorOf(PeersClient.props(etcPeerManager, peerEventBus, testSyncConfig, system.scheduler), "peers-client")

    lazy val ledger: Ledger =
      new LedgerImpl(bl, blockchainConfig, syncConfig, buildEthashConsensus, ExecutionContext.global)

    lazy val ommersPool: ActorRef = system.actorOf(OmmersPool.props(bl, 1), "ommers-pool")

    lazy val pendingTransactionsManager: ActorRef = system.actorOf(
      PendingTransactionsManager.props(TxPoolConfig(config), peerManager, etcPeerManager, peerEventBus),
      "pending-transactions-manager"
    )

    lazy val regularSync = system.actorOf(
      RegularSync.props(
        peersClient,
        etcPeerManager,
        peerEventBus,
        ledger,
        bl,
        blockchainConfig, // FIXME: remove in ETCM-280
        testSyncConfig,
        ommersPool,
        pendingTransactionsManager,
        checkpointBlockGenerator,
        system.scheduler
      )
    )

    def startRegularSync(): Task[Unit] = Task {
      regularSync ! SyncProtocol.Start
    }

    def broadcastBlock(
        blockNumber: Option[Int] = None
    )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] = {
      Task(blockNumber match {
        case Some(bNumber) =>
          bl.getBlockByNumber(bNumber).getOrElse(throw new RuntimeException(s"block by number: $bNumber doesn't exist"))
        case None => bl.getBestBlock()
      }).flatMap { block =>
        Task {
          val currentTd = bl
            .getTotalDifficultyByHash(block.hash)
            .getOrElse(throw new RuntimeException(s"block by hash: ${block.hash} doesn't exist"))
          val currentWolrd = getMptForBlock(block)
          val (newBlock, newTd, newWorld) = createChildBlock(block, currentTd, currentWolrd)(updateWorldForBlock)
          broadcastBlock(newBlock, newTd)
        }
      }
    }

    def waitForRegularSyncLoadLastBlock(blockNumer: BigInt): Task[Boolean] = {
      retryUntilWithDelay(Task(bl.getBestBlockNumber() == blockNumer), 1.second, 90) { isDone => isDone }
    }

    def mineNewBlock(
        plusDifficulty: BigInt = 0
    )(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] = Task {
      val block: Block = bl.getBestBlock()
      val currentTd = bl
        .getTotalDifficultyByHash(block.hash)
        .getOrElse(throw new RuntimeException(s"block by hash: ${block.hash} doesn't exist"))
      val currentWolrd = getMptForBlock(block)
      val (newBlock, newTd, newWorld) =
        createChildBlock(block, currentTd, currentWolrd, plusDifficulty)(updateWorldForBlock)
      regularSync ! SyncProtocol.MinedBlock(newBlock)
    }

    def mineNewBlocks(delay: FiniteDuration, nBlocks: Int)(
        updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy
    ): Task[Unit] = {
      if (nBlocks > 0) {
        mineNewBlock()(updateWorldForBlock)
          .delayExecution(delay)
          .flatMap(_ => mineNewBlocks(delay, nBlocks - 1)(updateWorldForBlock))
      } else Task(())
    }

    private def getMptForBlock(block: Block) = {
      bl.getWorldStateProxy(
        blockNumber = block.number,
        accountStartNonce = blockchainConfig.accountStartNonce,
        stateRootHash = Some(block.header.stateRoot),
        noEmptyAccounts = EvmConfig.forBlock(block.number, blockchainConfig).noEmptyAccounts,
        ethCompatibleStorage = blockchainConfig.ethCompatibleStorage
      )
    }

    private def broadcastBlock(block: Block, td: BigInt) = {
      broadcasterActor ! BroadcastBlock(NewBlock(block, td))
    }

    private def createChildBlock(
        parent: Block,
        parentTd: BigInt,
        parentWorld: InMemoryWorldStateProxy,
        plusDifficulty: BigInt = 0
    )(
        updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy
    ): (Block, BigInt, InMemoryWorldStateProxy) = {
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
      val newTd = newBlock.header.difficulty + parentTd
      (newBlock, newTd, parentWorld)
    }
  }

  object FakePeer {

    def startFakePeer(peerName: String, fakePeerCustomConfig: FakePeerCustomConfig): Task[FakePeer] = {
      for {
        peer <- Task(new FakePeer(peerName, fakePeerCustomConfig))
        _ <- peer.startPeer()
      } yield peer
    }

    def start1FakePeerRes(
        fakePeerCustomConfig: FakePeerCustomConfig = defaultConfig,
        name: String
    ): Resource[Task, FakePeer] = {
      Resource.make {
        startFakePeer(name, fakePeerCustomConfig)
      } { peer =>
        peer.shutdown()
      }
    }

    def start2FakePeersRes(
        fakePeerCustomConfig1: FakePeerCustomConfig = defaultConfig,
        fakePeerCustomConfig2: FakePeerCustomConfig = defaultConfig
    ): Resource[Task, (FakePeer, FakePeer)] = {
      for {
        peer1 <- start1FakePeerRes(fakePeerCustomConfig1, "Peer1")
        peer2 <- start1FakePeerRes(fakePeerCustomConfig2, "Peer2")
      } yield (peer1, peer2)
    }

  }
}
