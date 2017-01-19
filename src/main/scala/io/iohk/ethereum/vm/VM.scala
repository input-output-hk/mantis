package io.iohk.ethereum.vm

import scala.annotation.tailrec

class VM(program: Program) {


  def execute(): ProgramResult = {
    execute(ProgramState())
    ProgramResult()
  }

  @tailrec
  private def execute(state: ProgramState): ProgramState = {
    program.getOpCode(state.pc) match {
      case Some(opCode) =>
        val newState = opCode.execute(state)
        if (newState.halt)
          newState
        else
          execute(newState)

      case None => ???
    }
  }
}
