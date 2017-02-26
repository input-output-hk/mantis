package io.iohk.ethereum.vm

import io.iohk.ethereum.domain._


object ProgramContext {
  def apply(stx: SignedTransaction, blockHeader: BlockHeader, world: WorldStateProxy): ProgramContext = {
    import stx.tx

    val senderAddress = stx.recoveredSenderAddress.get // FIXME: get, it should be validated but...
    val (world1, recipientAddress, program) = callOrCreate(world, tx, senderAddress)

    val env = ExecEnv(recipientAddress, senderAddress, senderAddress, tx.gasPrice, tx.payload,
      tx.value, program, blockHeader, callDepth = 0)

    ProgramContext(env, tx.gasLimit, world1)
  }

  private def callOrCreate(world: WorldStateProxy, tx: Transaction, senderAddress: Address): (WorldStateProxy, Address, Program) = {
    if (tx.receivingAddress.isEmpty) {
      // contract create
      val (address, world1) = world.newAddress(senderAddress)
      val world2 = world1.saveAccount(address, Account.Empty)
      val world3 = world2.transfer(senderAddress, address, tx.value)
      val code = tx.payload

      (world3, address, Program(code))

    } else {
      // message call
      val world1 = world.transfer(senderAddress, tx.receivingAddress, tx.value)
      val account = world1.getGuaranteedAccount(tx.receivingAddress)
      val code = world1.getCode(account.codeHash)

      (world1, tx.receivingAddress, Program(code))
    }
  }

}
/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param env set of constants for the execution
  * @param startGas initial gas for the execution
  * @param world provides interactions with world state
  */
case class ProgramContext(
  env: ExecEnv,
  startGas: BigInt, //TODO: should we move it to ExecEnv
  world: WorldStateProxy)
