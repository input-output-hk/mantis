package io.iohk.ethereum.consensus.mining

case class FullMiningConfig[C <: AnyRef /*Product*/ ](
    generic: MiningConfig,
    specific: C
) {
  final def miningEnabled: Boolean = generic.miningEnabled
}
