package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address}

object ProgramState {
  def apply(context: ProgramContext): ProgramState =
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
case class ProgramState(
  context: ProgramContext,
  gas: BigInt,
  world: WorldStateProxy,
  stack: Stack = Stack.empty(),
  memory: Memory = Memory.empty,
  pc: Int = 0,
  returnData: ByteString = ByteString.empty,
  //TODO: investigate whether we need this or should refunds be simply added to current gas
  gasRefund: BigInt = 0,
  addressesToDelete: Seq[Address] = Seq(),
  halted: Boolean = false,
  error: Option[ProgramError] = None
) {

  def env: ExecEnv = context.env

  def ownAddress: Address = env.ownerAddr

  def ownAccount: Account = world.getGuaranteedAccount(ownAddress)

  def storage: Storage = world.getStorage(ownAccount.storageRoot)

  def gasUsed: BigInt = context.startGas - gas

  def withWorld(updated: WorldStateProxy): ProgramState =
    copy(world = updated)

  def withStorage(updated: Storage): ProgramState =
    withWorld(
      world
        .saveAccount(ownAddress, ownAccount.copy(storageRoot = updated.storageRoot))
        .saveStorage(updated.storageRoot, updated))

  def program: Program = env.program

  def inputData: ByteString = env.inputData

  def spendGas(amount: BigInt): ProgramState =
    copy(gas = gas - amount)

  def refundGas(amount: BigInt): ProgramState =
    copy(gasRefund = gasRefund + amount)

  def step(i: Int = 1): ProgramState =
    copy(pc = pc + i)

  def goto(i: Int): ProgramState =
    copy(pc = i)

  def withStack(stack: Stack): ProgramState =
    copy(stack = stack)

  def withMemory(memory: Memory): ProgramState =
    copy(memory = memory)

  def withError(error: ProgramError): ProgramState =
    copy(error = Some(error), halted = true)

  def withReturnData(data: ByteString): ProgramState =
    copy(returnData = data)

  def withAddressToDelete(addr: Address): ProgramState =
    copy(addressesToDelete = addressesToDelete :+ addr)

  def withAddressesToDelete(addresses: Seq[Address]): ProgramState =
    copy(addressesToDelete = addressesToDelete ++ addresses)

  def halt: ProgramState =
    copy(halted = true)
}
