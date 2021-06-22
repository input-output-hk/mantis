package io.iohk.ethereum.consensus.pow.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks.{BlockTimestampProvider, DefaultBlockTimestampProvider, PendingBlockAndState}
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.pow.RestrictedPoWSigner
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.domain.{Address, Block, BlockchainReader, SignedTransaction}
import io.iohk.ethereum.ledger.{BlockPreparator, InMemoryWorldStateProxy}
import io.iohk.ethereum.utils.BlockchainConfig
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import io.iohk.ethereum.consensus.ConsensusMetrics

class RestrictedPoWBlockGeneratorImpl(
    validators: ValidatorsExecutor,
    blockchainReader: BlockchainReader,
    blockchainConfig: BlockchainConfig,
    consensusConfig: ConsensusConfig,
    override val blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    minerKeyPair: AsymmetricCipherKeyPair,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends PoWBlockGeneratorImpl(
      validators,
      blockchainReader,
      blockchainConfig,
      consensusConfig,
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
  ): PendingBlockAndState = ConsensusMetrics.RestrictedPoWBlockGeneratorTiming.record { () =>
    val pHeader = parent.header
    val blockNumber = pHeader.number + 1
    val parentHash = pHeader.hash

    val validatedOmmers =
      validators.ommersValidator.validate(parentHash, blockNumber, ommers, blockchainReader) match {
        case Left(_) => emptyX
        case Right(_) => ommers
      }
    val prepared = prepareBlock(
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
