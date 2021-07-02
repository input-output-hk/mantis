package io.iohk.ethereum.vm

import akka.util.ByteString

import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.BlockHeader
import io.iohk.ethereum.domain.UInt256

object ExecEnv {
  def apply(context: ProgramContext[_, _], code: ByteString, ownerAddr: Address): ExecEnv = {
    import context._

    ExecEnv(
      ownerAddr,
      callerAddr,
      originAddr,
      gasPrice,
      inputData,
      value,
      Program(code),
      blockHeader,
      callDepth,
      startGas,
      evmConfig
    )
  }
}

//TODO: delete me
/** Execution environment constants of an EVM program.
  *  See section 9.3 in Yellow Paper for more detail.
  *  @param ownerAddr   I_a: address of the account that owns the code
  *  @param callerAddr  I_s: address of the account which caused the code to be executing
  *  @param originAddr  I_o: sender address of the transaction that originated this execution
  *  @param gasPrice    I_p
  *  @param inputData   I_d
  *  @param value       I_v
  *  @param program     I_b
  *  @param blockHeader I_H
  *  @param callDepth   I_e
  *  Extra:
  *  @param startGas    gas provided for execution
  *  @param evmConfig   EVM configuration (forks)
  */
case class ExecEnv(
    ownerAddr: Address,
    callerAddr: Address,
    originAddr: Address,
    gasPrice: UInt256,
    inputData: ByteString,
    value: UInt256,
    program: Program,
    blockHeader: BlockHeader,
    callDepth: Int,
    startGas: BigInt,
    evmConfig: EvmConfig
)
