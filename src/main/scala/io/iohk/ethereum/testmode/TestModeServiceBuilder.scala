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
    with BlockchainBuilder
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
      blockchainWriter,
      storagesInstance.storages.evmCodeStorage,
      syncConfig,
      scheduler,
      consensusConfig,
      vm,
      this
    )

  override lazy val stxLedger: StxLedger =
    testModeComponentsProvider.stxLedger(SealEngineType.NoReward)
}
