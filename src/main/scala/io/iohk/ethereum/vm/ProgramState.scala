package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

object ProgramState {
  def apply(context: ProgramContext): ProgramState =
    ProgramState(
      context = context,
      gas = context.startGas,
      storages = context.storages,
      balance = context.account.balance)
}

/**
  * Intermediate state updated with execution of each opcode in the program
  *
  * @param context the context which initiates the program
  * @param gas current gas for the execution
  * @param stack current stack
  * @param memory current memory
  * @param storages a map of accounts' storages to be modified
  * @param balance this contract's account balance
  * @param pc program counter - an index of the opcode in the program to be executed
  * @param returnData data to be returned from the program execution
  * @param gasRefund the amount of gas to be refunded after execution (not sure if a separate field is required)
  * @param internalTransfers list of transactions created during run of the program
  * @param addressesToDelete list of addresses of accounts scheduled to be deleted
  * @param halted a flag to indicate program termination
  * @param error indicates whether the program terminated abnormally
  */
case class ProgramState(
  context: ProgramContext,
  gas: BigInt,
  storages: Map[ByteString, Storage],
  balance: BigInt,
  stack: Stack = Stack.empty(),
  memory: Memory = Memory.empty,
  pc: Int = 0,
  returnData: ByteString = ByteString.empty,
  //TODO: investigate whether we need this or should refunds be simply added to current gas
  gasRefund: BigInt = 0,
  internalTransfers: List[Transfer] = Nil,
  addressesToDelete: Seq[Address] = Seq(),
  halted: Boolean = false,
  error: Option[ProgramError] = None
) {
  //** this contract's account storage
  val storage = context.storages(context.account.storageRoot)

  def env: ExecEnv = context.env

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

  def withStorage(storage: Storage): ProgramState =
    copy(storages = storages + (context.account.storageRoot -> storage))

  def withStorages(modifiedStorages: Map[ByteString, Storage]): ProgramState =
    copy(storages = storages ++ modifiedStorages)

  def withError(error: ProgramError): ProgramState =
    copy(error = Some(error), halted = true)

  def withReturnData(data: ByteString): ProgramState =
    copy(returnData = data)

  def transfer(to: Address, value: BigInt): ProgramState = {
    val t = Transfer(env.ownerAddr, to, value)
    copy(internalTransfers = t :: internalTransfers, balance = balance - value)
  }

  def withAddressToDelete(addr: Address): ProgramState =
    copy(addressesToDelete = addressesToDelete :+ addr)

  def halt: ProgramState =
    copy(halted = true)
}
