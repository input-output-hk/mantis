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
  def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    val finalState = run(ProgramState[W, S](context))
    ProgramResult[W, S](
      finalState.returnData,
      finalState.gas,
      finalState.world,
      finalState.addressesToDelete,
      finalState.logs,
      finalState.error,
      finalState.garbage)
  }

  @tailrec
  private def run[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val byte = state.program.getByte(state.pc)
    state.config.byteToOpCode.get(byte) match {
      case Some(opCode) =>
        val newState = opCode.execute(state)
        if (newState.halted)
          newState
        else
          run[W, S](newState)

      case None =>
        state.withError(InvalidOpCode(byte)).halt
    }
  }
}
