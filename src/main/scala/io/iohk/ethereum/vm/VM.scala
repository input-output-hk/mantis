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

  private def getOpCode(state: ProgramState): Either[ProgramError, OpCode] = getOpCode(state, state.pc)

  private def getOpCode(state: ProgramState, pos: Int): Either[ProgramError, OpCode] = {
    val byte = state.program.getByte(pos)
    OpCode.byteToOpCode.get(byte) match {
      case Some(opcode) => Right(opcode)
      case None => Left(InvalidOpCode(byte))
    }
  }

  def isJumpDest(state: ProgramState, pos: Int): Boolean = getOpCode(state, pos).contains(JUMPDEST)

}
