package io.iohk.ethereum.domain

import org.scalatest.flatspec.AnyFlatSpec

import io.iohk.ethereum.vm.Generators

class SignedLegacyTransactionSpec
    extends AnyFlatSpec
    with SignedTransactionBehavior {

  private def allowedPointSigns(chainId: Byte) = Set((chainId * 2 + 35).toByte, (chainId * 2 + 36).toByte)

  "Signed LegacyTransaction" should behave like SignedTransactionBehavior(Generators.legacyTransactionGen(), allowedPointSigns)
}
