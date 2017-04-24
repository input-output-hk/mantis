package io.iohk.ethereum.vm

import io.iohk.ethereum.utils.Logger

import scala.annotation.tailrec

/**
  * Entry point to executing a program.
  */
class VM extends Logger {

  /**
    * Executes a program
    * @param context context to be executed
    * @return result of the execution
   */
  def run[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramResult[W, S] = {
    PrecompiledContracts.runOptionally(context).getOrElse {
      val finalState = run(ProgramState[W, S](context))
      ProgramResult[W, S](
        finalState.returnData,
        finalState.gas,
        finalState.world,
        finalState.addressesToDelete,
        finalState.logs,
        finalState.gasRefund,
        finalState.error)
    }
  }

  @tailrec
  private def run[W <: WorldStateProxy[W, S], S <: Storage[S]](state: ProgramState[W, S]): ProgramState[W, S] = {
    val byte = state.program.getByte(state.pc)
    state.config.byteToOpCode.get(byte) match {
      case Some(opCode) =>
        val newState = opCode.execute(state)
        import newState._
        log.trace(s"$opCode | pc: $pc | depth: ${env.callDepth} | stack: ${stack}")
        if (newState.halted)
          newState
        else
          run[W, S](newState)

      case None =>
        state.withError(InvalidOpCode(byte)).halt
    }
  }
}

object VM extends VM
