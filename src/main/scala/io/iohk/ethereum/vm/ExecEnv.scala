package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{ Address, BlockHeader }

/** Execution environment constants of an EVM program.
 *  See section 9.3 in Yellow Paper for more detail.
*/
case class ExecEnv(ownerAddr: Address,
                   senderAddr: Address,
                   gasPrice: BigInt,
                   inputData: ByteString,
                   execAddr: Address,
                   value: BigInt,
                   program: Program,
                   blockHeader: BlockHeader,
                   callDepth: Int)
