package io.iohk.ethereum.consensus

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.validators.BlockHeaderValidator

/**
 * Abstraction for a consensus protocol.
 *
 * @see [[io.iohk.ethereum.consensus.Protocol Protocol]]
 */
// FIXME Lot's of stuff to do...
trait Consensus {
  type Config <: AnyRef /*Product*/

  def protocol: Protocol

  def config: FullConsensusConfig[Config]

  /** Returns `true` if this is the standard Ethereum PoW consensus protocol (`ethash`). */
  final def isEthash: Boolean = protocol.isEthash

  /**
   * Starts the consensus protocol on the current `node`.
   */
  def startProtocol(node: Node): Unit

  def stopProtocol(): Unit

  /**
   * Provides the [[io.iohk.ethereum.validators.BlockHeaderValidator BlockHeaderValidator]] that is specific
   * to this consensus protocol.
   */
  // FIXME Probably include the whole of [[io.iohk.ethereum.validators.Validators]].
  def blockHeaderValidator: BlockHeaderValidator

  // Ledger uses this in importBlock
  def validateBlockBeforeExecution(block: Block): Either[ValidationBeforeExecError, BlockExecutionSuccess] = ???
}
