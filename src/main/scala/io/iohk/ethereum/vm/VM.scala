package io.iohk.ethereum.vm

import scala.annotation.tailrec

import io.iohk.ethereum.utils.EitherExtensions._

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

  private def getOpCode(state: ProgramState): Either[ProgramError, OpCode] = {
    val byte = state.program.getByte(state.pc)
    OpCode.byteToOpCode.get(byte) match {
      case Some(opcode) => opcode.asRight
      case None => InvalidOpCode(byte).asLeft
    }
  }

}
