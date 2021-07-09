package io.iohk.ethereum.consensus.blocks

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.domain.SignedTransaction
import io.iohk.ethereum.ledger.InMemoryWorldStateProxy

/** We use a `BlockGenerator` to create the next block.
  * In a PoW setting, this is what a miner typically does.
  * In general, a [[BlockGenerator]] depends on and is provided by the
  * [[Mining Consensus]].
  *
  * @note This is generally a stateful object.
  * @see [[Mining#blockGenerator Consensus#blockGenerator()]],
  *      [[io.iohk.ethereum.ledger.BlockPreparator BlockPreparator]]
  */
trait BlockGenerator {

  /** The type of consensus-specific data used in the block generation process.
    * For example, under [[io.iohk.ethereum.consensus.pow.PoWMining EthashConsensus]],
    * this represents the [[io.iohk.ethereum.domain.BlockBody#uncleNodesList ommers]].
    */
  type X

  /** An empty `X` */
  def emptyX: X

  /** This function returns the block currently being mined block with highest timestamp
    */
  def getPendingBlock: Option[PendingBlock]

  def getPendingBlockAndState: Option[PendingBlockAndState]

  /** Generates the next block.
    */
  def generateBlock(
      parent: Block,
      transactions: Seq[SignedTransaction],
      beneficiary: Address,
      x: X,
      initialWorldStateBeforeExecution: Option[InMemoryWorldStateProxy]
  ): PendingBlockAndState
}

/** Internal API, used for testing.
  *
  * This is a [[BlockGenerator]] API for the needs of the test suites.
  */
trait TestBlockGenerator extends BlockGenerator {
  def blockTimestampProvider: BlockTimestampProvider

  def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): TestBlockGenerator
}
