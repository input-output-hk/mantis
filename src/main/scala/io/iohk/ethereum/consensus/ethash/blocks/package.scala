package io.iohk.ethereum.consensus.ethash

import io.iohk.ethereum.consensus.ethash.validators.OmmersValidator.OmmersError
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.ledger.BlockPreparationError

package object blocks {
  /**
   * This is type `X` in `BlockGenerator[X]`.
   *
   * @see [[io.iohk.ethereum.consensus.ethash.blocks.EthashBlockGenerator EthashBlockGenerator]],
   *      [[io.iohk.ethereum.consensus.blocks.BlockGenerator BlockGenerator{ type X} ]]
   */
  final type Ommers = Seq[BlockHeader]

  final case class InvalidOmmers(reason: OmmersError) extends BlockPreparationError
}
