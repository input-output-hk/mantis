package io.iohk.ethereum.testmode

import akka.util.ByteString

import monix.execution.Scheduler

import io.iohk.ethereum.consensus.Consensus
import io.iohk.ethereum.consensus.ConsensusImpl
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.BlockchainImpl
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.BlockchainWriter
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.nodebuilder.TestNode

/** Provides a ledger or consensus instances with modifiable blockchain config (used in test mode). */
class TestModeComponentsProvider(
    blockchain: BlockchainImpl,
    blockchainReader: BlockchainReader,
    blockchainWriter: BlockchainWriter,
    evmCodeStorage: EvmCodeStorage,
    validationExecutionContext: Scheduler,
    miningConfig: MiningConfig,
    vm: VMImpl,
    node: TestNode
) {

  def evaluateBranchBlock(
      preimageCache: collection.concurrent.Map[ByteString, UInt256]
  ): Consensus = {
    val consensuz = consensus()
    val blockValidation = new BlockValidation(consensuz, blockchainReader, node.blockQueue)
    val blockExecution =
      new TestModeBlockExecution(
        blockchain,
        blockchainReader,
        blockchainWriter,
        evmCodeStorage,
        consensuz.blockPreparator,
        blockValidation,
        (key: UInt256) => preimageCache.put(crypto.kec256(key.bytes), key)
      )

    new ConsensusImpl(
      blockchain,
      blockchainReader,
      blockchainWriter,
      node.blockQueue,
      blockValidation,
      blockExecution,
      validationExecutionContext
    )
  }

  /** Clear the internal builder state
    */
  def clearState(): Unit =
    node.blockQueue.clear()

  def consensus(
      blockTimestamp: Long = 0
  ): TestmodeMining =
    new TestmodeMining(
      vm,
      evmCodeStorage,
      blockchain,
      blockchainReader,
      miningConfig,
      node,
      blockTimestamp
    )
}
