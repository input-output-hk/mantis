package io.iohk.ethereum.vm

object GasCost {

}

sealed abstract class GasCost(val value: BigInt)

case object Zero extends GasCost(0)
case object VeryLow extends GasCost(2)
