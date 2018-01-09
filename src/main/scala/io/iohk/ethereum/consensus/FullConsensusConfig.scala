package io.iohk.ethereum.consensus

import io.iohk.ethereum.consensus.ethash.EthashConfig

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
   * This is a method that can handle such cases via a respective if/then/else construct:
   * if we run under [[io.iohk.ethereum.consensus.Protocol.Ethash Ethash]] the `_if` function is called,
   * otherwise the `_else` value is computed.
   */
  final def ifEthash[A](_if: EthashConfig ⇒ A)(_else: ⇒ A): A =
    specific match {
      case mc: ethash.EthashConfig ⇒ _if(mc)
      case _ ⇒ _else
    }

  final def isEthash: Boolean = generic.protocol.isEthash

  final def miningEnabled: Boolean = generic.miningEnabled
}
