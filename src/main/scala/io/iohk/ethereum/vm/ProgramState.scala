package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, TxLogEntry}

object ProgramState {
  def apply[W <: WorldStateProxy[W, S], S <: Storage[S]](context: ProgramContext[W, S]): ProgramState[W, S] =
    ProgramState(
      context = context,
      gas = context.startGas,
      world = context.world)
}

/**
  * Intermediate state updated with execution of each opcode in the program
  *
  * @param context the context which initiates the program
  * @param gas current gas for the execution
  * @param stack current stack
  * @param memory current memory
  * @param pc program counter - an index of the opcode in the program to be executed
  * @param returnData data to be returned from the program execution
  * @param gasRefund the amount of gas to be refunded after execution (not sure if a separate field is required)
  * @param addressesToDelete list of addresses of accounts scheduled to be deleted
  * @param halted a flag to indicate program termination
  * @param error indicates whether the program terminated abnormally
  */
case class ProgramState[W <: WorldStateProxy[W, S], S <: Storage[S]](
  context: ProgramContext[W, S],
  gas: UInt256,
  world: W,
  stack: Stack = Stack.empty(),
  memory: Memory = Memory.empty,
  pc: Int = 0,
  returnData: ByteString = ByteString.empty,
  //TODO: investigate whether we need this or should refunds be simply added to current gas
  gasRefund: UInt256 = 0,
  addressesToDelete: Seq[Address] = Seq(),
  logs: Vector[TxLogEntry] = Vector(),
  halted: Boolean = false,
  error: Option[ProgramError] = None
) {

  def env: ExecEnv = context.env

  def ownAddress: Address = env.ownerAddr

  def ownBalance: UInt256 = world.getBalance(ownAddress)

  def storage: S = world.getStorage(ownAddress)

  def gasUsed: UInt256 = context.startGas - gas

  def withWorld(updated: W): ProgramState[W, S] =
    copy(world = updated)

  def withStorage(updated: S): ProgramState[W, S] =
    withWorld(world.saveStorage(ownAddress, updated))

  def program: Program = env.program

  def inputData: ByteString = env.inputData

  def spendGas(amount: UInt256): ProgramState[W, S] =
    copy(gas = gas - amount)

  def refundGas(amount: UInt256): ProgramState[W, S] =
    copy(gasRefund = gasRefund + amount)

  def step(i: Int = 1): ProgramState[W, S] =
    copy(pc = pc + i)

  def goto(i: Int): ProgramState[W, S] =
    copy(pc = i)

  def withStack(stack: Stack): ProgramState[W, S] =
    copy(stack = stack)

  def withMemory(memory: Memory): ProgramState[W, S] =
    copy(memory = memory)

  def withError(error: ProgramError): ProgramState[W, S] =
    copy(error = Some(error), halted = true)

  def withReturnData(data: ByteString): ProgramState[W, S] =
    copy(returnData = data)

  def withAddressToDelete(addr: Address): ProgramState[W, S] =
    copy(addressesToDelete = addressesToDelete :+ addr)

  def withAddressesToDelete(addresses: Seq[Address]): ProgramState[W, S] =
    copy(addressesToDelete = addressesToDelete ++ addresses)

  def withLog(log: TxLogEntry): ProgramState[W, S] =
    copy(logs = logs :+ log)

  def halt: ProgramState[W, S] =
    copy(halted = true)
}
