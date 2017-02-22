package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.Address

case class Transfer(from: Address, to: Address, value: BigInt)
