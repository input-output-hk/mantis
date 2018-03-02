package io.iohk.ethereum.consensus.blocks

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Block, SignedTransaction}
import io.iohk.ethereum.ledger.BlockPreparationError

/**
 * A `BlockGenerator` is used by a miner in order to create the next block.
 * The definition of a miner is consensus-specific.
 *
 * @note This is generally a stateful object.
 *
 * @see [[io.iohk.ethereum.consensus.Consensus#blockGenerator() Consensus#blockGenerator()]],
 *      [[io.iohk.ethereum.ledger.BlockPreparator BlockPreparator]]
 *
 */
trait BlockGenerator {
  /**
   * The type of consensus-specific data used in the block generation process.
   * For example, under [[io.iohk.ethereum.consensus.ethash.EthashConsensus EthashConsensus]],
   * this represents the [[io.iohk.ethereum.network.p2p.messages.PV62.BlockBody#uncleNodesList() ommers]].
   *
   * @see [[io.iohk.ethereum.consensus.blocks.BlockGenerator#generateBlockForMining(io.iohk.ethereum.domain.Block, scala.collection.immutable.Seq, io.iohk.ethereum.domain.Address, java.lang.Object) BlockGenerator#generateBlockForMining]]
   */
  //noinspection ScalaStyle
  type X

  /** An empty `X` */
  def emptyX: X

  // FIXME This is currently used only for Ethash
  def getPrepared(powHeaderHash: ByteString): Option[PendingBlock]

  /**
   * This function returns the block currently being mined block with highest timestamp
   */
  def getPendingBlock: Option[PendingBlock]

  def getPendingBlockAndState: Option[PendingBlockAndState]

  def blockTimestampProvider: BlockTimestampProvider

  // FIXME This is currently used for testing
  def withBlockTimestampProvider(blockTimestampProvider: BlockTimestampProvider): BlockGenerator

  def generateBlockForMining(
    parent: Block,
    transactions: Seq[SignedTransaction],
    beneficiary: Address,
    ommers: X // we call it ommers to remember what is needed in Ethash but in general it can be anything
  ): Either[BlockPreparationError, PendingBlock]
}
