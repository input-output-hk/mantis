package io.iohk.ethereum.vm

import scala.annotation.tailrec

/**
  * Entry point to executing a program.
  */
object VM {

  /**
    * Executes a program
    * @param context context to be executed
    * @return result of the execution
   */
  def run(context: ProgramContext): ProgramResult = {
    val finalState = run(ProgramState(context))
    ProgramResult(
      finalState.returnData,
      finalState.gas,
      context.startGas - finalState.gas,
      finalState.storages,
      finalState.internalTransfers.reverse,
      finalState.addressesToDelete,
      finalState.error)
  }

  @tailrec
  private def run(state: ProgramState): ProgramState = {
    val byte = state.program.getByte(state.pc)
    OpCode.byteToOpCode.get(byte) match {
      case Some(opcode) =>
        val newState = opcode.execute(state)
        if (newState.halted)
          newState
        else
          run(newState)

      case None =>
        state.withError(InvalidOpCode(byte)).halt
    }
  }
}
