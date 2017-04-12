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

    val gasLimit = tx.gasLimit - config.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit, blockHeader.number)

    ProgramContext(env, UInt256(gasLimit), world1, config)
  }

  private def callOrCreate[W <: WorldStateProxy[W, S], S <: Storage[S]](world: W, tx: Transaction, senderAddress: Address): (W, Address, Program) = {
    tx.receivingAddress match {
      case None =>
        // contract create
        val address = world.createAddress(senderAddress)
        val world2 = world.newEmptyAccount(address)
        val world3 = world2.transfer(senderAddress, address, UInt256(tx.value))
        val code = tx.payload

        (world3, address, Program(code))

      case Some(txReceivingAddress) =>
        // message call
        val worldAfterTransfer = world.transfer(senderAddress, txReceivingAddress, UInt256(tx.value))
        val code = worldAfterTransfer.getCode(txReceivingAddress)

        (worldAfterTransfer, tx.receivingAddress.get, Program(code))
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
  * @param config evm config
  */
case class ProgramContext[W <: WorldStateProxy[W, S], S <: Storage[S]](
  env: ExecEnv,
  startGas: UInt256, //TODO: should we move it to ExecEnv
  world: W,
  config: EvmConfig)
