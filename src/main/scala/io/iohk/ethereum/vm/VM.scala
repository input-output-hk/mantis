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
  // TODO We'll have to pass additional parameters to this method.
  // Please create a ProgramContext class and keep storage inside of it.
  def run(env: ExecEnv, storage: Storage): ProgramResult = {
    val finalState = run(ProgramState(env, storage))
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
