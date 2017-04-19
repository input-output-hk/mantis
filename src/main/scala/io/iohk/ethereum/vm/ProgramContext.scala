package io.iohk.ethereum.vm

import io.iohk.ethereum.domain._


object ProgramContext {
  def apply[W <: WorldStateProxy[W, S], S <: Storage[S]](
    stx: SignedTransaction,
    blockHeader: BlockHeader,
    world: W,
    config: EvmConfig): ProgramContext[W, S] = {

    import stx.tx

    val senderAddress = stx.senderAddress
    val (world1, recipientAddress, program) = callOrCreate[W, S](world, tx, senderAddress)

    val env = ExecEnv(recipientAddress, senderAddress, senderAddress, UInt256(tx.gasPrice), tx.payload,
      UInt256(tx.value), program, blockHeader, callDepth = 0)

    val gasLimit = tx.gasLimit - config.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit)

    ProgramContext(env, recipientAddress, UInt256(gasLimit), world1, config)
  }

  private def callOrCreate[W <: WorldStateProxy[W, S], S <: Storage[S]](world: W, tx: Transaction, senderAddress: Address): (W, Address, Program) = {
    tx.receivingAddress match {
      case None =>
        // contract create
        val address = world.createAddress(senderAddress)
        val world1 = world.newEmptyAccount(address)
        val world2 = world1.transfer(senderAddress, address, UInt256(tx.value))
        val code = tx.payload

        (world2, address, Program(code))

      case Some(txReceivingAddress) =>
        // message call
        val worldAfterTransfer = world.transfer(senderAddress, txReceivingAddress, UInt256(tx.value))
        val code = worldAfterTransfer.getCode(txReceivingAddress)

        (worldAfterTransfer, txReceivingAddress, Program(code))
    }
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
  startGas: UInt256,
  world: W,
  config: EvmConfig)
