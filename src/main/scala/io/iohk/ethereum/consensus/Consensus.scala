package io.iohk.ethereum.consensus

import io.iohk.ethereum.domain.Block
import io.iohk.ethereum.ledger.BlockExecutionError.ValidationBeforeExecError
import io.iohk.ethereum.ledger.BlockExecutionSuccess
import io.iohk.ethereum.nodebuilder.Node
import io.iohk.ethereum.validators.BlockHeaderValidator

/**
 * Provides generic requirements from an abstracted consensus protocol.
 * Finer-grained consensus details can be hidden in the specific implementation.
 */
// FIXME Lot's of stuff to do...
trait Consensus {
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
