package io.iohk.ethereum.consensus.ethash.blocks

import java.util.function.UnaryOperator
import akka.util.ByteString
import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.ethash.validators.ValidatorsExecutor
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparator, InMemoryWorldStateProxy}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.consensus.ConsensusMetrics

/** Internal API, used for testing (especially mocks) */
trait EthashBlockGenerator extends TestBlockGenerator {
  type X = Ommers

  /** An empty `X` */
  def emptyX: Ommers

  def getPrepared(powHeaderHash: ByteString): Option[PendingBlock]
}

class EthashBlockGeneratorImpl(
    validators: ValidatorsExecutor,
    blockchain: Blockchain,
    blockchainConfig: BlockchainConfig,
    consensusConfig: ConsensusConfig,
    val blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends BlockGeneratorSkeleton(
      blockchain,
      blockchainConfig,
      consensusConfig,
      difficultyCalc,
      blockTimestampProvider
    )
    with EthashBlockGenerator {

  protected def newBlockBody(transactions: Seq[SignedTransaction], x: Ommers): BlockBody =
    BlockBody(transactions, x)

  protected def prepareHeader(
      blockNumber: BigInt,
      parent: Block,
      beneficiary: Address,
      blockTimestamp: Long,
      x: Ommers
  ): BlockHeader =
    defaultPrepareHeader(blockNumber, parent, beneficiary, blockTimestamp, x)

  /** An empty `X` */
  def emptyX: Ommers = Nil

  def getPrepared(powHeaderHash: ByteString): Option[PendingBlock] =
    ConsensusMetrics.MinedBlockEvaluationTimer.record { () =>
      cache
        .getAndUpdate(new UnaryOperator[List[PendingBlockAndState]] {
          override def apply(t: List[PendingBlockAndState]): List[PendingBlockAndState] =
            t.filterNot(pbs =>
              ByteString(kec256(BlockHeader.getEncodedWithoutNonce(pbs.pendingBlock.block.header))) == powHeaderHash
            )
        })
        .find { pbs =>
          ByteString(kec256(BlockHeader.getEncodedWithoutNonce(pbs.pendingBlock.block.header))) == powHeaderHash
        }
        .map(_.pendingBlock)
    }

  def generateBlock(
      parent: Block,
      transactions: Seq[SignedTransaction],
      beneficiary: Address,
      x: Ommers,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy]
  ): PendingBlockAndState = ConsensusMetrics.EthashBlockGeneratorTiming.record { () =>
    val pHeader = parent.header
    val blockNumber = pHeader.number + 1
    val parentHash = pHeader.hash

    val ommers = validators.ommersValidator.validate(parentHash, blockNumber, x, blockchain) match {
      case Left(_) => emptyX
      case Right(_) => x
    }

    val prepared = prepareBlock(
      parent,
      transactions,
      beneficiary,
      blockNumber,
      blockPreparator,
      ommers,
      initialWorldStateBeforeExecution
    )

    cache.updateAndGet { t: List[PendingBlockAndState] =>
      (prepared :: t).take(blockCacheSize)
    }

    prepared
  }

  def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): EthashBlockGeneratorImpl =
    new EthashBlockGeneratorImpl(
      validators,
      blockchain,
      blockchainConfig,
      consensusConfig,
      blockPreparator,
      difficultyCalc,
      blockTimestampProvider
    )
}
