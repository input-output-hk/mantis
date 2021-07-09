package io.iohk.ethereum.consensus

case class FullConsensusConfig[C <: AnyRef /*Product*/ ](
    generic: MiningConfig,
    specific: C
) {
  final def miningEnabled: Boolean = generic.miningEnabled
}
