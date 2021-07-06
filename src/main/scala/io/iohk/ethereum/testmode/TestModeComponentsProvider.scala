package io.iohk.ethereum.testmode

import akka.util.ByteString

import monix.execution.Scheduler

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.ledger.BlockImport
import io.iohk.ethereum.ledger.BlockQueue
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.ledger.StxLedger
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig

/** Provides a ledger or consensus instances with modifiable blockchain config (used in test mode). */
class TestModeComponentsProvider(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    evmCodeStorage: EvmCodeStorage,
    syncConfig: SyncConfig,
    validationExecutionContext: Scheduler,
    consensusConfig: ConsensusConfig,
    difficultyCalculator: DifficultyCalculator,
    vm: VMImpl
) {

//  private var cache = HashMap.empty[(BlockchainConfig, SealEngineType), BlockImport]
  private val internalBlockQueue = BlockQueue(blockchain, syncConfig)

  def blockQueue(): BlockQueue = internalBlockQueue

  def blockImport(
      blockchainConfig: BlockchainConfig,
      preimageCache: collection.concurrent.Map[ByteString, UInt256],
      sealEngine: SealEngineType
  ): BlockImport = {
//    val blockQueue = BlockQueue(blockchain, syncConfig)
    val consensuz = consensus(blockchainConfig, sealEngine)
    val blockValidation = new BlockValidation(consensuz, blockchainReader, internalBlockQueue)
    val blockExecution =
      new TestModeBlockExecution(
        blockchain,
        blockchainReader,
        evmCodeStorage,
        blockchainConfig,
        consensuz.blockPreparator,
        blockValidation,
        (key: UInt256) => preimageCache.put(crypto.kec256(key.bytes), key)
      )

    new BlockImport(
      blockchain,
      blockchainReader,
      internalBlockQueue,
      blockValidation,
      blockExecution,
      validationExecutionContext
    )
  }

  /** Clear the internal builder state
    */
  def clearState(): Unit =
//    blockQueue = BlockQueue(blockchain, syncConfig)
//    cache = cache.empty
    internalBlockQueue.clear()

  def stxLedger(blockchainConfig: BlockchainConfig, sealEngine: SealEngineType): StxLedger =
    new StxLedger(
      blockchain,
      blockchainReader,
      evmCodeStorage,
      blockchainConfig,
      consensus(blockchainConfig, sealEngine).blockPreparator
    )
  def consensus(
      blockchainConfig: BlockchainConfig,
      sealEngine: SealEngineType,
      blockTimestamp: Long = 0
  ): TestmodeConsensus =
    new TestmodeConsensus(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      blockchainConfig,
      consensusConfig,
      difficultyCalculator,
      sealEngine,
      blockTimestamp
    )
}
