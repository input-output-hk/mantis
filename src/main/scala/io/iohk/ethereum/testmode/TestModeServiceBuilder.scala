package io.iohk.ethereum.testmode

import monix.execution.Scheduler

import io.iohk.ethereum.consensus.ConsensusBuilder
import io.iohk.ethereum.consensus.ConsensusConfigBuilder
import io.iohk.ethereum.ledger._
import io.iohk.ethereum.nodebuilder.ActorSystemBuilder
import io.iohk.ethereum.nodebuilder._

trait TestModeServiceBuilder extends StxLedgerBuilder {
  self: BlockchainConfigBuilder
    with StorageBuilder
    with TestBlockchainBuilder
    with SyncConfigBuilder
    with ConsensusBuilder
    with ActorSystemBuilder
    with ConsensusConfigBuilder
    with BlockQueueBuilder
    with VmBuilder =>

  val scheduler: Scheduler = Scheduler(system.dispatchers.lookup("validation-context"))

  lazy val testModeComponentsProvider: TestModeComponentsProvider =
    new TestModeComponentsProvider(
      blockchain,
      blockchainReader,
      storagesInstance.storages.evmCodeStorage,
      syncConfig,
      scheduler,
      consensusConfig,
      vm,
      this
    )

  override lazy val blockQueue: BlockQueue = testModeComponentsProvider.blockQueue();

//<<<<<<< HEAD
//=======
//  private def testLedger: Ledger = testModeComponentsProvider.ledger(blockchainConfig, SealEngineType.NoReward)
//
//  class TestLedgerProxy extends Ledger {
//    override def consensus: Consensus = testLedger.consensus
//    override def checkBlockStatus(blockHash: ByteString): BlockStatus = testLedger.checkBlockStatus(blockHash)
//    override def getBlockByHash(hash: ByteString): Option[Block] = testLedger.getBlockByHash(hash)
//    override def importBlock(block: Block)(implicit
//        blockExecutionScheduler: Scheduler
//    ): Task[BlockImportResult] = testLedger.importBlock(block)
//    override def resolveBranch(headers: NonEmptyList[BlockHeader]): BranchResolutionResult =
//      testLedger.resolveBranch(headers)
//    override def getChainWeightByHash(hash: ByteString): Option[ChainWeight] = testLedger.getChainWeightByHash(hash)
//  }
//
//  override lazy val ledger: Ledger = new TestLedgerProxy
//>>>>>>> f521a3125 ([ETCM-927] enhance BlockchainTests/ValidBlocks/bcMultiChainTest/ChainAtoChainB_difficultyB test)
  override lazy val stxLedger: StxLedger =
    testModeComponentsProvider.stxLedger(blockchainConfig, SealEngineType.NoReward)
}
