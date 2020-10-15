package io.iohk.ethereum.sync.util

import akka.actor.{ActorRef, ActorSystem}
import cats.effect.Resource
import io.iohk.ethereum.blockchain.sync.PeersClient
import io.iohk.ethereum.blockchain.sync.regular.RegularSync
import io.iohk.ethereum.consensus.{ConsensusConfig, FullConsensusConfig, ethash}
import io.iohk.ethereum.consensus.ethash.{EthashConfig, EthashConsensus}
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.ledger.{InMemoryWorldStateProxy, Ledger, LedgerImpl}
import io.iohk.ethereum.nodebuilder.VmSetup
import io.iohk.ethereum.ommers.OmmersPool
import io.iohk.ethereum.sync.util.SyncUtils.{ShutdownHookBuilder, ValidatorsExecutorAlwaysSucceed, retryUntilWithDelay}
import io.iohk.ethereum.transactions.PendingTransactionsManager
import io.iohk.ethereum.utils.{Config, TxPoolConfig, VmConfig}
import monix.eval.Task

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class FakePeerRegularSync(peerName: String) extends FakePeer {

  override implicit protected def system: ActorSystem = ActorSystem(peerName)

  override def start: Task[Unit] = Task {
    regularSync ! RegularSync.Start
  }

  def buildEthashConsensus(): ethash.EthashConsensus = {
    val consensusConfig: ConsensusConfig = ConsensusConfig(Config.config)(ShutdownHookBuilder)
    val specificConfig: EthashConfig = ethash.EthashConfig(config)
    val fullConfig = FullConsensusConfig(consensusConfig, specificConfig)
    val vm =  VmSetup.vm(VmConfig(config), blockchainConfig, testMode = false)
    val consensus = EthashConsensus(vm, bl, blockchainConfig, fullConfig, ValidatorsExecutorAlwaysSucceed)
    consensus
  }

  lazy val peersClient: ActorRef = system.actorOf(PeersClient.props(etcPeerManager,
    peerEventBus,
    testSyncConfig,
    system.scheduler), "peers-client")

  lazy val ledger: Ledger = new LedgerImpl(bl, blockchainConfig, syncConfig, buildEthashConsensus, ExecutionContext.global)

  lazy val ommersPool: ActorRef = system.actorOf(OmmersPool.props(bl,
    1),
    "ommers-pool")

  lazy val pendingTransactionsManager: ActorRef = system.actorOf(
    PendingTransactionsManager.props(TxPoolConfig(config),
      peerManager,
      etcPeerManager,
      peerEventBus),
    "pending-transactions-manager" )

  lazy val regularSync = system.actorOf(
    RegularSync.props(
      peersClient,
      etcPeerManager,
      peerEventBus,
      ledger,
      bl,
      testSyncConfig,
      ommersPool,
      pendingTransactionsManager,
      system.scheduler
    )
  )

  def waitForRegularSyncLoadLastBlock(blockNumer: BigInt): Task[Boolean] = {
    retryUntilWithDelay(
      Task(bl.getBestBlockNumber() == blockNumer), 1.second,90) { isDone => isDone }
  }

  def mineNewBlock(plusDifficulty: BigInt = 0)(updateWorldForBlock: (BigInt, InMemoryWorldStateProxy) => InMemoryWorldStateProxy): Task[Unit] = Task {
    val block: Block = bl.getBestBlock()
    val currentTd = bl.getTotalDifficultyByHash(block.hash).getOrElse(throw new RuntimeException(s"block by hash: ${block.hash} doesn't exist"))
    val currentWolrd = getMptForBlock(block)
    val (newBlock, newTd, newWorld) = createChildBlock(block, currentTd, currentWolrd, plusDifficulty)(updateWorldForBlock)
    regularSync ! RegularSync.MinedBlock(newBlock)
  }

}

object FakePeerRegularSync {

  def startFakePeer(peerName: String): Task[FakePeerRegularSync] = {
    for {
      peer <- Task(new FakePeerRegularSync(peerName))
      _ <- peer.startPeer()
    } yield peer
  }

  def start2FakePeersRes(): Resource[Task, (FakePeerRegularSync, FakePeerRegularSync)] = {
    Resource.make {
      Task.parZip2(startFakePeer("Peer1"), startFakePeer("Peer2"))
    } { case (peer, peer1) => Task.parMap2(peer.shutdown(), peer1.shutdown())((_, _) => ()) }
  }

}