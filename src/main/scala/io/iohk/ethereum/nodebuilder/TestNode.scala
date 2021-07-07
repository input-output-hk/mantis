package io.iohk.ethereum.nodebuilder

import java.util.concurrent.atomic.AtomicReference

import monix.execution.Scheduler

import io.iohk.ethereum.jsonrpc.TestService
import io.iohk.ethereum.ledger.StxLedger
import io.iohk.ethereum.testmode.SealEngineType
import io.iohk.ethereum.testmode.TestEthBlockServiceWrapper
import io.iohk.ethereum.testmode.TestModeComponentsProvider
import io.iohk.ethereum.testmode.TestmodeConsensus
import io.iohk.ethereum.utils.BlockchainConfig

class TestNode extends BaseNode {

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

  override lazy val ethBlocksService =
    new TestEthBlockServiceWrapper(blockchain, blockchainReader, consensus, blockQueue)

  override lazy val stxLedger: StxLedger =
    testModeComponentsProvider.stxLedger(SealEngineType.NoReward)

  override lazy val consensus = new TestmodeConsensus(
    vm,
    storagesInstance.storages.evmCodeStorage,
    blockchain,
    blockchainReader,
    consensusConfig,
    SealEngineType.NoReward
  )
  override lazy val testService: Option[TestService] =
    Some(
      new TestService(
        blockchain,
        blockchainReader,
        blockchainWriter,
        storagesInstance.storages.stateStorage,
        storagesInstance.storages.evmCodeStorage,
        pendingTransactionsManager,
        consensusConfig,
        testModeComponentsProvider,
        storagesInstance.storages.transactionMappingStorage,
        this
      )(scheduler)
    )

  private lazy val currentBlockchainConfig = new AtomicReference(initBlockchainConfig)

  implicit override def blockchainConfig: BlockchainConfig = currentBlockchainConfig.get()

  def setBlockchainConfig(config: BlockchainConfig): Unit = currentBlockchainConfig.set(config)
}
