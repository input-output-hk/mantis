package io.iohk.ethereum.testmode

import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl, StxLedger}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

/** Provides a ledger instance with modifiable blockchain config (used in test mode). */
class TestServiceProvider(
    blockchain: BlockchainImpl,
    syncConfig: SyncConfig,
    consensus: Consensus,
    validationExecutionContext: Scheduler
) {

  def ledger(blockchainConfig: BlockchainConfig): Ledger = new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus, validationExecutionContext)
  def stxLedger(blockchainConfig: BlockchainConfig): StxLedger = new StxLedger(blockchain, blockchainConfig, consensus.blockPreparator)
}
