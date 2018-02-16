package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.ethash.MiningConfig

case class FullConsensusConfig[C <: AnyRef /*Product*/](
  generic: ConsensusConfig,
  specific: C
) {
  // Sanity check
  require(
    generic.protocol.isEthash == ifEthash(_ ⇒ true)(false),
    "generic.protocol.isEthash == ifEthash(_ ⇒ true)(false)"
  )

  /**
   * There are APIs that expect that the standard Ethash consensus is running and so depend
   * on either its configuration or general PoW semantics.
   * This is a method that can handle such cases via a respective if/then/else construct.
   */
  final def ifEthash[A](_if: MiningConfig ⇒ A)(_else: ⇒ A): A =
    specific match {
      case mc: ethash.MiningConfig ⇒ _if(mc)
      case _ ⇒ _else
    }

  final def isEthash: Boolean = generic.protocol.isEthash

  final def miningEnabled: Boolean = generic.miningEnabled
}
