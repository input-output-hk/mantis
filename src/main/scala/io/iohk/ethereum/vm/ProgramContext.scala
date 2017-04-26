package io.iohk.ethereum.vm

import akka.util.ByteString
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

    // YP eq (91)
    val inputData =
      if(tx.isContractInit) ByteString.empty
      else tx.payload

    val senderAddress = stx.senderAddress

    val env = ExecEnv(recipientAddress, senderAddress, senderAddress, UInt256(tx.gasPrice), inputData,
      UInt256(tx.value), program, blockHeader, callDepth = 0)

    val gasLimit = tx.gasLimit - config.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit)

    ProgramContext(env, recipientAddress, gasLimit, world, config)
  }
}

/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param env set of constants for the execution
  * @param receivingAddr used for determining whether a precompiled contract is being called (potentially
  *                      different from the addresses defined in env)
  * @param startGas initial gas for the execution
  * @param world provides interactions with world state
  * @param config evm config
  */
case class ProgramContext[W <: WorldStateProxy[W, S], S <: Storage[S]](
  env: ExecEnv,
  receivingAddr: Address,
  startGas: BigInt,
  world: W,
  config: EvmConfig)
