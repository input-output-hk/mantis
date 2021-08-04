package io.iohk.ethereum.nodebuilder

import java.util.concurrent.atomic.AtomicReference

import monix.execution.Scheduler

import io.iohk.ethereum.jsonrpc.TestService
import io.iohk.ethereum.testmode.SealEngineType
import io.iohk.ethereum.testmode.TestEthBlockServiceWrapper
import io.iohk.ethereum.testmode.TestModeComponentsProvider
import io.iohk.ethereum.testmode.TestmodeMining
import io.iohk.ethereum.utils.BlockchainConfig

class TestNode extends BaseNode {

  val scheduler: Scheduler = Scheduler(system.dispatchers.lookup("validation-context"))

  lazy val testModeComponentsProvider: TestModeComponentsProvider =
    new TestModeComponentsProvider(
      blockchain,
      blockchainReader,
      blockchainWriter,
      storagesInstance.storages.evmCodeStorage,
      scheduler,
      miningConfig,
      vm,
      this
    )

  override lazy val ethBlocksService =
    new TestEthBlockServiceWrapper(blockchain, blockchainReader, mining, blockQueue)

  override lazy val mining = new TestmodeMining(
    vm,
    storagesInstance.storages.evmCodeStorage,
    blockchain,
    blockchainReader,
    miningConfig,
    this
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
        miningConfig,
        testModeComponentsProvider,
        storagesInstance.storages.transactionMappingStorage,
        this
      )(scheduler)
    )

  lazy val currentBlockchainConfig: AtomicReference[BlockchainConfig] = new AtomicReference(initBlockchainConfig)
  implicit override def blockchainConfig: BlockchainConfig = currentBlockchainConfig.get()

  val currentSealEngine: AtomicReference[SealEngineType] = new AtomicReference(SealEngineType.NoReward)
  def sealEngine: SealEngineType = currentSealEngine.get()

}
