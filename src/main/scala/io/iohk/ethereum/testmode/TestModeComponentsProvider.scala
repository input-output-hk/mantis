package io.iohk.ethereum.testmode

import akka.util.ByteString
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.{Consensus, ConsensusConfig}
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.{BlockchainImpl, BlockchainReader, UInt256}
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.ledger.StxLedger
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.utils.Config.SyncConfig
import monix.execution.Scheduler
import io.iohk.ethereum.ledger.BlockImport
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.ledger.BlockExecution
import io.iohk.ethereum.ledger.BlockQueue

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

  def blockImport(
      blockchainConfig: BlockchainConfig,
      preimageCache: collection.concurrent.Map[ByteString, UInt256],
      sealEngine: SealEngineType
  ): BlockImport = {
    val blockQueue = BlockQueue(blockchain, syncConfig)
    val consensuz = consensus(blockchainConfig, sealEngine)
    val blockValidation = new BlockValidation(consensuz, blockchainReader, blockQueue)
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
      blockQueue,
      blockValidation,
      blockExecution,
      validationExecutionContext
    )
  }

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
