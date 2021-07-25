package io.iohk.ethereum.consensus.pow.blocks

import java.util.function.UnaryOperator

import akka.util.ByteString

import io.iohk.ethereum.consensus.blocks._
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.consensus.mining.MiningConfig
import io.iohk.ethereum.consensus.mining.MiningMetrics
import io.iohk.ethereum.consensus.pow.validators.ValidatorsExecutor
import io.iohk.ethereum.crypto.kec256
import io.iohk.ethereum.db.storage.EvmCodeStorage
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy
import io.iohk.ethereum.utils.BlockchainConfig

/** Internal API, used for testing (especially mocks) */
trait PoWBlockGenerator extends TestBlockGenerator {
  type X = Ommers

  /** An empty `X` */
  def emptyX: Ommers

  def getPrepared(powHeaderHash: ByteString): Option[PendingBlock]
}

class PoWBlockGeneratorImpl(
    evmCodeStorage: EvmCodeStorage,
    validators: ValidatorsExecutor,
    blockchainReader: BlockchainReader,
    miningConfig: MiningConfig,
    val blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends BlockGeneratorSkeleton(
      miningConfig,
      difficultyCalc,
      blockTimestampProvider
    )
    with PoWBlockGenerator {

  protected def newBlockBody(transactions: Seq[SignedTransaction], x: Ommers): BlockBody =
    BlockBody(transactions, x)

  protected def prepareHeader(
      blockNumber: BigInt,
      parent: Block,
      beneficiary: Address,
      blockTimestamp: Long,
      x: Ommers
  )(implicit blockchainConfig: BlockchainConfig): BlockHeader =
    defaultPrepareHeader(blockNumber, parent, beneficiary, blockTimestamp, x)

  /** An empty `X` */
  def emptyX: Ommers = Nil

  def getPrepared(powHeaderHash: ByteString): Option[PendingBlock] =
    MiningMetrics.MinedBlockEvaluationTimer.record { () =>
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
  )(implicit blockchainConfig: BlockchainConfig): PendingBlockAndState = MiningMetrics.PoWBlockGeneratorTiming.record {
    () =>
      val pHeader = parent.header
      val blockNumber = pHeader.number + 1
      val parentHash = pHeader.hash

      val ommers = validators.ommersValidator.validate(parentHash, blockNumber, x, blockchainReader) match {
        case Left(_)  => emptyX
        case Right(_) => x

      }
      val prepared = prepareBlock(
        evmCodeStorage,
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

  def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): PoWBlockGeneratorImpl =
    new PoWBlockGeneratorImpl(
      evmCodeStorage,
      validators,
      blockchainReader,
      miningConfig,
      blockPreparator,
      difficultyCalc,
      blockTimestampProvider
    )
}
