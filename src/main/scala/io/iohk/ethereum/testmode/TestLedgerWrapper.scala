package io.iohk.ethereum.testmode

import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.Ledger
import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.ledger.StxLedger
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler

/** Provides a ledger instance with modifiable blockchain config (used in test mode). */
class TestLedgerWrapper(
    blockchain: BlockchainImpl,
    syncConfig: SyncConfig,
    consensus: Consensus,
    var blockchainConfig: BlockchainConfig, // var as it's modifiable by test_ RPC endpoints
    validationExecutionContext: Scheduler
) {

  def ledger: Ledger = new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus, validationExecutionContext)
  def stxLedger: StxLedger = new StxLedger(blockchain, blockchainConfig, consensus.blockPreparator)
}
