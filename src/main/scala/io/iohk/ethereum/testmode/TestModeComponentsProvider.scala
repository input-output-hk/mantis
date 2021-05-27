package io.iohk.ethereum.testmode

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.{Consensus, ConsensusConfig}
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl, StxLedger}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

/** Provides a ledger instance with modifiable blockchain config (used in test mode). */
class TestModeComponentsProvider(
    blockchain: BlockchainImpl,
    syncConfig: SyncConfig,
    validationExecutionContext: Scheduler,
    consensusConfig: ConsensusConfig,
    difficultyCalculator: DifficultyCalculator,
    vm: VMImpl
) {

  def ledger(blockchainConfig: BlockchainConfig): Ledger =
    new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus(blockchainConfig), validationExecutionContext)
  def stxLedger(blockchainConfig: BlockchainConfig): StxLedger =
    new StxLedger(blockchain, blockchainConfig, consensus(blockchainConfig).blockPreparator)
  def consensus(blockchainConfig: BlockchainConfig, blockTimestamp: Long = 0): TestmodeConsensus =
    new TestmodeConsensus(vm, blockchain, blockchainConfig, consensusConfig, difficultyCalculator, blockTimestamp)
}
