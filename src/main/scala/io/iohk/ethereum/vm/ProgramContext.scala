package io.iohk.ethereum.vm

import io.iohk.ethereum.domain.{BlockHeader, SignedTransaction}


object ProgramContext {
  def apply(stx: SignedTransaction, blockHeader: BlockHeader, world: WorldStateProxy): ProgramContext = {
    import stx.tx._
    val account = world.getAccount(receivingAddress).get // FIXME: get, but an account must be created prior to TX execution
    val senderAddress = stx.recoveredSenderAddress.get // FIXME: get, it should be validated but...
    val program = Program(world.getCode(account.codeHash))

    val env = ExecEnv(receivingAddress, senderAddress, gasPrice, payload, senderAddress,
      value, program, blockHeader, callDepth = 0)

    ProgramContext(env, gasLimit, world)
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
