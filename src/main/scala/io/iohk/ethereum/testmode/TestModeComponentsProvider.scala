package io.iohk.ethereum.testmode

import akka.util.ByteString
import monix.execution.Scheduler
import io.iohk.ethereum.consensus.ConsensusAdapter
import io.iohk.ethereum.consensus.ConsensusImpl
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.crypto
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.{BlockMetadataProxy, BlockchainImpl, BlockchainReader, BlockchainWriter, UInt256}
import io.iohk.ethereum.ledger.BlockValidation
import io.iohk.ethereum.ledger.VMImpl
import io.iohk.ethereum.nodebuilder.TestNode

/** Provides a ledger or consensus instances with modifiable blockchain config (used in test mode). */
class TestModeComponentsProvider(
    blockchain: BlockchainImpl,
    blockMetadataProxy: BlockMetadataProxy,
    blockchainReader: BlockchainReader,
    blockchainWriter: BlockchainWriter,
    evmCodeStorage: EvmCodeStorage,
    validationExecutionContext: Scheduler,
    miningConfig: MiningConfig,
    vm: VMImpl,
    node: TestNode
) {

  def getConsensus(
      preimageCache: collection.concurrent.Map[ByteString, UInt256]
  ): ConsensusAdapter = {
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

    new ConsensusAdapter(
      new ConsensusImpl(
        blockchain,
        blockMetadataProxy,
        blockchainReader,
        blockchainWriter,
        blockExecution
      ),
      blockchainReader,
      node.blockQueue,
      blockValidation,
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
