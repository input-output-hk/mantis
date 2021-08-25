package io.iohk.ethereum.domain

import io.iohk.ethereum.vm.Generators
import org.scalatest.flatspec.AnyFlatSpec

class SignedTransactionWithAccessListSpec
  extends AnyFlatSpec
    with SignedTransactionBehavior {

  private def allowedPointSigns(chainId: Byte) = Set(0.toByte, 1.toByte)

  "Signed TransactionWithAccessList" should behave like SignedTransactionBehavior(Generators.typedTransactionGen(), allowedPointSigns)
}
