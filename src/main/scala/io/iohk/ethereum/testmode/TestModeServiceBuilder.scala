package io.iohk.ethereum.testmode

import akka.util.ByteString
import cats.data.NonEmptyList
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.{Consensus, ConsensusBuilder, ConsensusConfigBuilder}
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.nodebuilder.{ActorSystemBuilder, _}
import monix.eval.Task
import monix.execution.Scheduler

trait TestModeServiceBuilder extends LedgerBuilder {
  self: BlockchainConfigBuilder
    with StorageBuilder
    with TestBlockchainBuilder
    with SyncConfigBuilder
    with ConsensusBuilder
    with ActorSystemBuilder
    with ConsensusConfigBuilder
    with VmBuilder =>

  val scheduler = Scheduler(system.dispatchers.lookup("validation-context"))

  lazy val testModeComponentsProvider: TestModeComponentsProvider =
    new TestModeComponentsProvider(
      blockchain,
      storagesInstance.storages.evmCodeStorage,
      syncConfig,
      scheduler,
      consensusConfig,
      DifficultyCalculator(blockchainConfig),
      vm
    )

  private def testLedger: Ledger = testModeComponentsProvider.ledger(blockchainConfig, SealEngineType.NoReward)

  class TestLedgerProxy extends Ledger {
    override def consensus: Consensus = testLedger.consensus
    override def checkBlockStatus(blockHash: ByteString): BlockStatus = testLedger.checkBlockStatus(blockHash)
    override def getBlockByHash(hash: ByteString): Option[Block] = testLedger.getBlockByHash(hash)
    override def importBlock(block: Block)(implicit
        blockExecutionScheduler: Scheduler
    ): Task[BlockImportResult] = testLedger.importBlock(block)
    override def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult =
      testLedger.resolveBranch(headers)
  }

  override lazy val ledger: Ledger = new TestLedgerProxy
  override lazy val stxLedger: StxLedger =
    testModeComponentsProvider.stxLedger(blockchainConfig, SealEngineType.NoReward)
}
