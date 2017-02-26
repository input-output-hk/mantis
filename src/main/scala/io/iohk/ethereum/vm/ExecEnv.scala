package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{ Address, BlockHeader }

/** Execution environment constants of an EVM program.
  *  See section 9.3 in Yellow Paper for more detail.
  *  @param ownerAddr address of accounts that owns the code
  */
case class ExecEnv(ownerAddr: Address,
                   callerAddr: Address,
                   originAddr: Address,
                   gasPrice: BigInt,
                   inputData: ByteString,
                   value: BigInt,
                   program: Program,
                   blockHeader: BlockHeader,
                   callDepth: Int)
