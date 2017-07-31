package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

//TODO: scaladoc
case class InternalTransaction(
  opcode: OpCode,
  from: Address,
  to: Option[Address],
  gasLimit: BigInt,
  data: ByteString,
  value: BigInt
)
