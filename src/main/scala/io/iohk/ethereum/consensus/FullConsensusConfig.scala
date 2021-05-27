package io.iohk.ethereum.consensus

case class FullConsensusConfig[C <: AnyRef /*Product*/ ](
    generic: ConsensusConfig,
    specific: C
) {
  final def miningEnabled: Boolean = generic.miningEnabled
  final def miningOnDemand: Boolean = generic.miningOnDemand
}
