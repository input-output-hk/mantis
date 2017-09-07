package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

/**
  * This class may be used for tracing any internal calls (*CALL*, CREATE) during code execution.
  * Currently it's only in Ethereum Test Suite (ets)
  *
  * @param opcode - the opcode that caused the internal TX
  * @param from - the account that executes the opcode
  * @param to - the account to which the call was made
  * @param gasLimit - gas available to the sub-execution
  * @param data - call data
  * @param value - call value
  */
case class InternalTransaction(
  opcode: OpCode,
  from: Address,
  to: Option[Address],
  gasLimit: BigInt,
  data: ByteString,
  value: BigInt
)
