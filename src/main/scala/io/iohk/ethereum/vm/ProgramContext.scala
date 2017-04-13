package io.iohk.ethereum.vm

import io.iohk.ethereum.domain._


object ProgramContext {
  def apply[W <: WorldStateProxy[W, S], S <: Storage[S]](
    stx: SignedTransaction,
    recipientAddress: Address,
    program: Program,
    blockHeader: BlockHeader,
    world: W,
    config: EvmConfig): ProgramContext[W, S] = {

    import stx.tx

    val senderAddress = stx.senderAddress

    val env = ExecEnv(recipientAddress, senderAddress, senderAddress, UInt256(tx.gasPrice), tx.payload,
      UInt256(tx.value), program, blockHeader, callDepth = 0)

    val gasLimit = tx.gasLimit - config.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit, blockHeader.number)

    ProgramContext(env, UInt256(gasLimit), world, config)
  }
}

/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param env set of constants for the execution
  * @param startGas initial gas for the execution
  * @param world provides interactions with world state
  * @param config evm config
  */
case class ProgramContext[W <: WorldStateProxy[W, S], S <: Storage[S]](
  env: ExecEnv,
  startGas: UInt256, //TODO: should we move it to ExecEnv
  world: W,
  config: EvmConfig)
