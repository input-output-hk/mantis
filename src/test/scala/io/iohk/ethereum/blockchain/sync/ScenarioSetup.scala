package io.iohk.ethereum.blockchain.sync

import io.iohk.ethereum.Mocks
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.consensus.{Consensus, StdConsensusBuilder}
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.ledger.LedgerImpl
import io.iohk.ethereum.nodebuilder.{LedgerBuilder, SyncConfigBuilder}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig
import io.iohk.ethereum.vm.VM

/**
 * Provides a standard setup for the test suites.
 * The reference to "cake" is about the "Cake Pattern" used in Mantis, especially
 * regarding the creation and wiring of the several components of a
 * [[io.iohk.ethereum.nodebuilder.Node Node]].
 */
trait ScenarioSetup extends StdConsensusBuilder with SyncConfigBuilder with LedgerBuilder {
  // Give a more specific type to Ledger, it is needed by the tests
  override lazy val ledger: LedgerImpl = newLedger()

  protected val successValidators: Validators = Mocks.MockValidatorsAlwaysSucceed
  protected val failureValidators: Validators = Mocks.MockValidatorsAlwaysFail

  /**
   * Reuses the existing consensus instance and creates a new one
   * by overriding its `validators` and `vm`.
   *
   * @note The existing consensus instance is provided lazily via the cake, so that at the moment
   *       of this call it may well have been overridden.
   *
   * @note Do not use this call in order to override the existing consensus instance because you will
   *       introduce circularity.
   *
   * @note The existing consensus instance will continue to live independently and will still be
   *       the instance provided by the cake.
   */
  protected def newTestConsensus(validators: Validators = consensus.validators, vm: VM = consensus.vm): Consensus =
    consensus.withValidators(validators.asInstanceOf[consensus.Validators]).withVM(vm)

  /**
   * Creates a new ledger instance, independent of the instance provided by the cake.
   *
   * @note Since ledger depends on the consensus, it is the caller's responsibility to
   *       make sure that the cake-provided consensus and the one used here do not interfere.
   *       In particular, after the new ledger is created, then the authoritative consensus for the ledger
   *       is the one returned by [[io.iohk.ethereum.ledger.Ledger#consensus() Ledger#consensus]], not the
   *       one provided by the cake.
   *
   * @note You can use this method to override the existing ledger instance that is provided by the cake.
   */
  protected def newTestLedger(
    blockchain: BlockchainImpl = blockchain,
    blockchainConfig: BlockchainConfig = blockchainConfig,
    syncConfig: SyncConfig = syncConfig,
    consensus: Consensus = consensus
  ): LedgerImpl =
    new LedgerImpl(
      blockchain = blockchain,
      blockchainConfig = blockchainConfig,
      syncConfig = syncConfig,
      theConsensus = consensus
    )

  protected def newTestLedger(validators: Validators = consensus.validators, vm: VM = consensus.vm): LedgerImpl = {
    val newConsensus = newTestConsensus(validators = validators, vm = vm)
    newTestLedger(consensus = newConsensus)
  }
}
