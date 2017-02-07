package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

/** Execution environment constants of an EVM program */
case class ExecEnv(ownerAddr: Address,
                   senderAddr: Address,
                   gasPrice: BigInt,
                   inputData: ByteString,
                   execAddr: Address,
                   value: BigInt,
                   program: Program,
                   blockHeader: Any,
                   callDepth: Int)
