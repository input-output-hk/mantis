package io.iohk.ethereum.ets.common

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.vm.UInt256

case class ScenarioGroup[T](
  name: String,
  scenarios: Map[String, T]
)

case class AccountState(
  balance: BigInt,
  nonce: BigInt,
  code: ByteString,
  storage: Map[UInt256, UInt256]
)

