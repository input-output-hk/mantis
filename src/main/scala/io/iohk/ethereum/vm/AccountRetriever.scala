package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.{Account, Address}

trait AccountRetriever {
  def getAccount(address: Address): Option[Account]
}
