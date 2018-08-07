package io.iohk.ethereum.testmode

import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.{Ledger, LedgerImpl}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig

/** Provides a ledger instance with modifiable blockchain config (used in test-mode).*/
class TestLedgerWrapper(
    blockchain: BlockchainImpl,
    syncConfig: SyncConfig,
    consensus: Consensus,
    var blockchainConfig: BlockchainConfig) { // var as it's modifiable by test_ RPC endpoints

  def ledger: Ledger = new LedgerImpl(blockchain, blockchainConfig, syncConfig, consensus)
}
