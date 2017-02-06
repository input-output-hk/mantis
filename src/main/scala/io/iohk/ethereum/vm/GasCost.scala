package io.iohk.ethereum.vm

// scalastyle:off magic.number
object GasCost {

}

sealed abstract class GasCost(val value: BigInt)

case object GZero extends GasCost(0)
case object GBase extends GasCost(2)
case object GVeryLow extends GasCost(3)
case object GLow extends GasCost(5)
case object GMid extends GasCost(8)
case object GHigh extends GasCost(10)
case object GExp extends GasCost(10)
case object GExpByte extends GasCost(10)
