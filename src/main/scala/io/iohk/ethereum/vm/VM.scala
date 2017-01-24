package io.iohk.ethereum.vm

import cats.syntax.either._
import scala.annotation.tailrec

object VM {

  def execute(invoke: ProgramInvoke): ProgramResult = {
    val finalState = execute(ProgramState(invoke))
    ProgramResult(finalState.returnData, finalState.storage, finalState.error)
  }

  @tailrec
  private def execute(state: ProgramState): ProgramState = {
    getOpCode(state) match {
      case Right(opcode) =>
        val newState = opcode.execute(state)
        if (newState.halted)
          newState
        else
          execute(newState)

      case Left(error) =>
        state.withError(error).halt
    }
  }

  private def getOpCode(state: ProgramState): Either[ProgramError, OpCode] =
    state.program.getByte(state.pc).right.flatMap { byte =>
      OpCode.byteToOpCode.get(byte) match {
        case Some(opcode) => opcode.asRight
        case None => InvalidOpCode(byte).asLeft
      }
    }
}
