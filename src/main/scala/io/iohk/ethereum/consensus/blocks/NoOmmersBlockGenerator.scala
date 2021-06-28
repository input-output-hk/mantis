package io.iohk.ethereum.consensus.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.{BlockPreparator, InMemoryWorldStateProxy}
import io.iohk.ethereum.utils.BlockchainConfig
import io.iohk.ethereum.consensus.ConsensusMetrics
import io.iohk.ethereum.db.storage.EvmCodeStorage

abstract class NoOmmersBlockGenerator(
    evmCodeStorage: EvmCodeStorage,
    blockchainConfig: BlockchainConfig,
    consensusConfig: ConsensusConfig,
    blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends BlockGeneratorSkeleton(
      blockchainConfig,
      consensusConfig,
      difficultyCalc,
      blockTimestampProvider
    ) {

  type X = Nil.type

  protected def newBlockBody(transactions: Seq[SignedTransaction], x: Nil.type): BlockBody =
    BlockBody(transactions, x)

  protected def prepareHeader(
      blockNumber: BigInt,
      parent: Block,
      beneficiary: Address,
      blockTimestamp: Long,
      x: Nil.type
  ): BlockHeader =
    defaultPrepareHeader(blockNumber, parent, beneficiary, blockTimestamp, x)

  /** An empty `X` */
  def emptyX: Nil.type = Nil

  def generateBlock(
      parent: Block,
      transactions: Seq[SignedTransaction],
      beneficiary: Address,
      x: Nil.type,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy]
  ): PendingBlockAndState = ConsensusMetrics.NoOmmersBlockGeneratorTiming.record { () =>
    val pHeader = parent.header
    val blockNumber = pHeader.number + 1

    val prepared =
      prepareBlock(
        evmCodeStorage,
        parent,
        transactions,
        beneficiary,
        blockNumber,
        blockPreparator,
        x,
        initialWorldStateBeforeExecution
      )
    cache.updateAndGet((t: List[PendingBlockAndState]) => (prepared :: t).take(blockCacheSize))

    prepared
  }
}
