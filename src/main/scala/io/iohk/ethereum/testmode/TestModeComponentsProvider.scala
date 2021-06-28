package io.iohk.ethereum.testmode

import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.{Consensus, ConsensusConfig}
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.Ledger.VMImpl
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl, StxLedger}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

/** Provides a ledger or consensus instances with modifiable blockchain config (used in test mode). */
class TestModeComponentsProvider(
    blockchain: BlockchainImpl,
    evmCodeStorage: EvmCodeStorage,
    syncConfig: SyncConfig,
    validationExecutionContext: Scheduler,
    consensusConfig: ConsensusConfig,
    difficultyCalculator: DifficultyCalculator,
    vm: VMImpl
) {

  def ledger(blockchainConfig: BlockchainConfig, sealEngine: SealEngineType): Ledger =
    new LedgerImpl(
      blockchain,
      evmCodeStorage,
      blockchainConfig,
      syncConfig,
      consensus(blockchainConfig, sealEngine),
      validationExecutionContext
    )
  def stxLedger(blockchainConfig: BlockchainConfig, sealEngine: SealEngineType): StxLedger =
    new StxLedger(blockchain, evmCodeStorage, blockchainConfig, consensus(blockchainConfig, sealEngine).blockPreparator)
  def consensus(
      blockchainConfig: BlockchainConfig,
      sealEngine: SealEngineType,
      blockTimestamp: Long = 0
  ): TestmodeConsensus =
    new TestmodeConsensus(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainConfig,
      consensusConfig,
      difficultyCalculator,
      sealEngine,
      blockTimestamp
    )
}
