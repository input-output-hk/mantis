package io.iohk.ethereum.consensus.blocks

import io.iohk.ethereum.consensus.ConsensusConfig
import io.iohk.ethereum.consensus.difficulty.DifficultyCalculator
import io.iohk.ethereum.domain._
import io.iohk.ethereum.ledger.BlockPreparator
import io.iohk.ethereum.utils.BlockchainConfig

abstract class NoOmmersBlockGenerator(
    blockchain: Blockchain,
    blockchainConfig: BlockchainConfig,
    consensusConfig: ConsensusConfig,
    blockPreparator: BlockPreparator,
    difficultyCalc: DifficultyCalculator,
    blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
) extends BlockGeneratorSkeleton(
      blockchain,
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
      x: Nil.type
  ): PendingBlock = {

    val pHeader = parent.header
    val blockNumber = pHeader.number + 1

    val prepared = prepareBlock(parent, transactions, beneficiary, blockNumber, blockPreparator, x)
    cache.updateAndGet((t: List[PendingBlockAndState]) => (prepared :: t).take(blockCacheSize))

    prepared.pendingBlock
  }
}
