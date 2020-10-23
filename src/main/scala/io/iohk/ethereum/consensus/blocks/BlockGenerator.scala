package io.iohk.ethereum.consensus.blocks

import io.iohk.ethereum.domain.{Address, Block, SignedTransaction}

/**
  * We use a `BlockGenerator` to create the next block.
  * In a PoW setting, this is what a miner typically does.
  * In general, a [[BlockGenerator]] depends on and is provided by the
  * [[io.iohk.ethereum.consensus.Consensus Consensus]].
  *
  * @note This is generally a stateful object.
  *
  * @see [[io.iohk.ethereum.consensus.Consensus#blockGenerator Consensus#blockGenerator()]],
  *      [[io.iohk.ethereum.ledger.BlockPreparator BlockPreparator]]
  */
trait BlockGenerator {

  /**
    * The type of consensus-specific data used in the block generation process.
    * For example, under [[io.iohk.ethereum.consensus.ethash.EthashConsensus EthashConsensus]],
    * this represents the [[io.iohk.ethereum.domain.BlockBody#uncleNodesList ommers]].
    */
  type X

  /** An empty `X` */
  def emptyX: X

  /**
    * This function returns the block currently being mined block with highest timestamp
    */
  def getPendingBlock: Option[PendingBlock]

  def getPendingBlockAndState: Option[PendingBlockAndState]

  /**
    * Generates the next block.
    */
  def generateBlock(
      parent: Block,
      transactions: Seq[SignedTransaction],
      beneficiary: Address,
      x: X
  ): PendingBlock
}

/**
  * Internal API, used for testing.
  *
  * This is a [[BlockGenerator]] API for the needs of the test suites.
  */
trait TestBlockGenerator extends BlockGenerator {
  def blockTimestampProvider: BlockTimestampProvider

  def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): TestBlockGenerator
}
