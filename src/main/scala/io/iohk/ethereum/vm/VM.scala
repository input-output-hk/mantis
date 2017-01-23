package io.iohk.ethereum.vm

import scala.annotation.tailrec

object VM {

  def execute(program: Program): ProgramResult = {
    val finalState = execute(ProgramState(program))
    ProgramResult(finalState.returnData, finalState.storage)
  }

  @tailrec
  private def execute(state: ProgramState): ProgramState = {
    getOpCode(state) match {
      case Right(opcode) =>
        val newState = opcode.execute(state)
        if (newState.halt)
          newState
        else
          execute(newState)

      case Left(error) =>
        state.copy(halt = true, error = Some(error))
    }
  }

  private def getOpCode(state: ProgramState): Either[ProgramError, OpCode] =
    state.program.getByte(state.pc).right.flatMap { byte =>
      OpCode.byteToOpCode.get(byte) match {
        case Some(opcode) => Right(opcode)
        case None => Left(InvalidOpCode(byte))
      }
    }
}
