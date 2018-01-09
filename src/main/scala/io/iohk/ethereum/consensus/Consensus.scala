package io.iohk.ethereum.consensus

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
   * This call must be made before `startMiningProcess`.
   */
  def startProtocol(node: Node): Unit

  /**
   * Starts the mining process on the current `node`.
   * It is up to the consensus protocol to define the semantics of mining.
   */
  def startMiningProcess(node: Node): Unit

  /**
   * Provides the [[io.iohk.ethereum.validators.BlockHeaderValidator BlockHeaderValidator]] that is specific
   * to this consensus protocol.
   */
  // FIXME Probably include the whole of [[io.iohk.ethereum.validators.Validators]].
  def blockHeaderValidator: BlockHeaderValidator
}
