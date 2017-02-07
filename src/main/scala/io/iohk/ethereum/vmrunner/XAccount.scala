package io.iohk.ethereum.vmrunner

import io.iohk.ethereum.domain.{Account, Address}

case class XAccount(acc: Account, name: String, address: Address, abis: Seq[ABI]) {
  def updateBalance(value: BigInt): XAccount = {
    val newBalance = acc.balance + value
    copy(acc = acc.copy(balance = newBalance))
  }
}
