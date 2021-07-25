package io.iohk.ethereum.consensus.pow.blocks

import org.bouncycastle.crypto.AsymmetricCipherKeyPair

import io.iohk.ethereum.consensus.blocks.BlockTimestampProvider
import io.iohk.ethereum.consensus.blocks.DefaultBlockTimestampProvider
import io.iohk.ethereum.consensus.blocks.PendingBlockAndState
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.consensus.mining.MiningMetrics
import io.iohk.ethereum.consensus.pow.RestrictedPoWSigner
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.BlockchainReader
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.utils.BlockchainConfig

class RestrictedPoWBlockGeneratorImpl(
    evmCodeStorage: EvmCodeStorage,
    validators: ValidatorsExecutor,
    blockchainReader: BlockchainReader,
    miningConfig: MiningConfig,
    override val blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    minerKeyPair: AsymmetricCipherKeyPair,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends PoWBlockGeneratorImpl(
      evmCodeStorage,
      validators,
      blockchainReader,
      miningConfig,
      blockPreparator,
      difficultyCalc,
      blockTimestampProvider
    ) {

  override def generateBlock(
      parent: Block,
      transactions: Seq[SignedTransaction],
      beneficiary: Address,
      ommers: Ommers,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy]
  )(implicit blockchainConfig: BlockchainConfig): PendingBlockAndState =
    MiningMetrics.RestrictedPoWBlockGeneratorTiming.record { () =>
      val pHeader = parent.header
      val blockNumber = pHeader.number + 1
      val parentHash = pHeader.hash

      val validatedOmmers =
        validators.ommersValidator.validate(parentHash, blockNumber, ommers, blockchainReader) match {
          case Left(_)  => emptyX
          case Right(_) => ommers
        }
      val prepared = prepareBlock(
        evmCodeStorage,
        parent,
        transactions,
        beneficiary,
        blockNumber,
        blockPreparator,
        validatedOmmers,
        initialWorldStateBeforeExecution
      )
      val preparedHeader = prepared.pendingBlock.block.header
      val headerWithAdditionalExtraData = RestrictedPoWSigner.signHeader(preparedHeader, minerKeyPair)
      val modifiedPrepared = prepared.copy(pendingBlock =
        prepared.pendingBlock.copy(block = prepared.pendingBlock.block.copy(header = headerWithAdditionalExtraData))
      )

      cache.updateAndGet { t: List[PendingBlockAndState] =>
        (modifiedPrepared :: t).take(blockCacheSize)
      }

      modifiedPrepared
    }

}
