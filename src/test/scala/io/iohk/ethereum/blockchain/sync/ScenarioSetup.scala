package io.iohk.ethereum.blockchain.sync

import java.util.concurrent.Executors

import monix.execution.Scheduler

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor

import io.iohk.ethereum.Mocks
import io.iohk.ethereum.Mocks.MockVM
import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.Protocol
import io.iohk.ethereum.consensus.StdTestConsensusBuilder
import io.iohk.ethereum.consensus.TestConsensus
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.consensus.validators.Validators
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockImport
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.nodebuilder._

/** Provides a standard setup for the test suites.
  * The reference to "cake" is about the "Cake Pattern" used in Mantis.
  * Specifically it relates to the creation and wiring of the several components of a
  * [[io.iohk.ethereum.nodebuilder.Node Node]].
  */
trait ScenarioSetup extends StdTestConsensusBuilder with StxLedgerBuilder {
  protected lazy val executionContextExecutor: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))
  protected lazy val monixScheduler: Scheduler = Scheduler(executionContextExecutor)
  protected lazy val successValidators: Validators = Mocks.MockValidatorsAlwaysSucceed
  protected lazy val failureValidators: Validators = Mocks.MockValidatorsAlwaysFail
  protected lazy val powValidators: ValidatorsExecutor = ValidatorsExecutor(Protocol.PoW)

  /** The default validators for the test cases.
    * Override this if you want to alter the behaviour of consensus
    * or if you specifically want other validators than the consensus provides.
    *
    * @note If you override this, consensus will pick up automatically.
    */
  lazy val validators: Validators = successValidators

  //+ cake overrides
  /** The default VM for the test cases.
    */
  override lazy val vm: VMImpl = new MockVM()

  /** The default consensus for the test cases.
    * We redefine it here in order to take into account different validators and vm
    * that a test case may need.
    *
    * @note We use the refined type [[io.iohk.ethereum.consensus.TestConsensus TestConsensus]]
    *       instead of just [[io.iohk.ethereum.consensus.Consensus Consensus]].
    *
    * @note If you override this, consensus will pick up automatically.
    */
  override lazy val consensus: TestConsensus = buildTestConsensus().withValidators(validators).withVM(vm)

  /** Reuses the existing consensus instance and creates a new one
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
  protected def newTestConsensus(validators: Validators = consensus.validators, vm: VMImpl = consensus.vm): Consensus =
    consensus.withValidators(validators).withVM(vm)

  protected def mkBlockImport(
      validators: Validators = validators,
      blockExecutionOpt: Option[BlockExecution] = None
  ): BlockImport = {
    val consensuz = consensus.withValidators(validators).withVM(new Mocks.MockVM())
    val blockValidation = new BlockValidation(consensuz, blockchainReader, blockQueue)
    new BlockImport(
      blockchain,
      blockchainReader,
      blockQueue,
      blockValidation,
      blockExecutionOpt.getOrElse(
        new BlockExecution(
          blockchain,
          blockchainReader,
          storagesInstance.storages.evmCodeStorage,
          consensuz.blockPreparator,
          blockValidation
        )
      ),
      Scheduler(system.dispatchers.lookup("validation-context"))
    )
  }

}
