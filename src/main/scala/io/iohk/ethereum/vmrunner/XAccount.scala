package io.iohk.ethereum.vmrunner

import io.iohk.ethereum.domain.{Account, Address}

case class XAccount(acc: Account, name: String, address: Address, abis: Seq[ABI])
