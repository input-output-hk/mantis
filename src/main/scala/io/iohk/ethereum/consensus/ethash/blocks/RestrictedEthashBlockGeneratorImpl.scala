package io.iohk.ethereum.consensus.ethash.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks.{BlockTimestampProvider, DefaultBlockTimestampProvider, PendingBlockAndState}
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.RestrictedEthashSigner
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.domain.{Address, Block, Blockchain, SignedTransaction}
import io.iohk.ethereum.ledger.{BlockPreparator, InMemoryWorldStateProxy}
import io.iohk.ethereum.utils.BlockchainConfig
import org.bouncycastle.crypto.AsymmetricCipherKeyPair
import io.iohk.ethereum.consensus.ConsensusMetrics

class RestrictedEthashBlockGeneratorImpl(
    validators: ValidatorsExecutor,
    blockchain: Blockchain,
    blockchainConfig: BlockchainConfig,
    consensusConfig: ConsensusConfig,
    override val blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    minerKeyPair: AsymmetricCipherKeyPair,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends EthashBlockGeneratorImpl(
      validators,
      blockchain,
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
  ): PendingBlockAndState = ConsensusMetrics.RestrictedEthashBlockGeneratorTiming.record { () =>
    val pHeader = parent.header
    val blockNumber = pHeader.number + 1
    val parentHash = pHeader.hash

    val validatedOmmers = validators.ommersValidator.validate(parentHash, blockNumber, ommers, blockchain) match {
      case Left(_)  => emptyX
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
    val headerWithAdditionalExtraData = RestrictedEthashSigner.signHeader(preparedHeader, minerKeyPair)
    val modifiedPrepared = prepared.copy(pendingBlock =
      prepared.pendingBlock.copy(block = prepared.pendingBlock.block.copy(header = headerWithAdditionalExtraData))
    )

    cache.updateAndGet { t: List[PendingBlockAndState] =>
      (modifiedPrepared :: t).take(blockCacheSize)
    }

    modifiedPrepared
  }

}
