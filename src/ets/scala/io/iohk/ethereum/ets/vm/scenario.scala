package io.iohk.ethereum.ets.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.vm.UInt256

case class ScenarioGroup(
  name: String,
  scenarios: Map[String, Scenario]
)

case class Scenario(
  env: Env,
  exec: Exec,
  callcreates: Option[List[CallCreate]],
  pre: Map[Address, AccountState],
  post: Option[Map[Address, AccountState]],
  logs: Option[List[LogEntry]],
  gas: Option[BigInt],
  out: Option[ByteString]
)

case class Env(
  currentCoinbase: Address,
  currentDifficulty: BigInt,
  currentGasLimit: BigInt,
  currentNumber: BigInt,
  currentTimestamp: Long,
  previousHash: Option[ByteString]
)

case class Exec(
  address: Address,
  origin: Address,
  caller: Address,
  value: BigInt,
  data: ByteString,
  code: ByteString,
  gasPrice: BigInt,
  gas: BigInt
)

case class AccountState(
  balance: BigInt,
  nonce: BigInt,
  code: ByteString,
  storage: Map[UInt256, UInt256]
)

case class CallCreate(
  data: ByteString,
  destination: Option[Address],
  gasLimit: BigInt,
  value: BigInt
)

case class LogEntry(
  address: Address,
  data: ByteString,
  topics: List[ByteString]
)

