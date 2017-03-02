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
      finalState.storage,
      finalState.internalTransactions,
      finalState.addressesToDelete,
      finalState.error)
  }

  @tailrec
  private def run(state: ProgramState): ProgramState = {
    getOpCode(state) match {
      case Right(opcode) =>
        val newState = opcode.execute(state)
        if (newState.halted)
          newState
        else
          run(newState)

      case Left(error) =>
        state.withError(error).halt
    }
  }

  private def getOpCode(state: ProgramState): Either[ProgramError, OpCode] = getOpCode(state.program, state.pc)

  private[vm] def getOpCode(program: Program, pos: Int): Either[ProgramError, OpCode] = {
    val byte = program.getByte(pos)
    OpCode.byteToOpCode.get(byte) match {
      case Some(opcode) => Right(opcode)
      case None => Left(InvalidOpCode(byte))
    }
  }

}
