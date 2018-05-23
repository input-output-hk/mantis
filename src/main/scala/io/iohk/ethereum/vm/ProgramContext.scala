package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain._

object ProgramContext {
  def apply[W <: WorldStateProxy[W, S], S <: Storage[S]](
    stx: SignedTransaction,
    blockHeader: BlockHeader,
    world: W,
    evmConfig: EvmConfig): ProgramContext[W, S] = {

    import stx.tx
    val gasLimit = tx.gasLimit - evmConfig.calcTransactionIntrinsicGas(tx.payload, tx.isContractInit)

    ProgramContext(
      callerAddr = stx.senderAddress,
      originAddr = stx.senderAddress,
      recipientAddr = tx.receivingAddress,
      gasPrice = UInt256(tx.gasPrice),
      startGas = gasLimit,
      inputData = tx.payload,
      value = UInt256(tx.value),
      endowment = UInt256(tx.value),
      doTransfer = true,
      blockHeader = blockHeader,
      callDepth = 0,
      world = world,
      initialAddressesToDelete = Set(),
      evmConfig = evmConfig
    )
  }
}

/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * Execution constants, see section 9.3 in Yellow Paper for more detail.
  * @param callerAddr  I_s: address of the account which caused the code to be executing
  * @param originAddr  I_o: sender address of the transaction that originated this execution
  * @param gasPrice    I_p
  * @param inputData   I_d
  * @param value       I_v
  * @param blockHeader I_H
  * @param callDepth   I_e
  *
  * Additional parameters:
  * @param recipientAddr recipient of the call, empty if contract creation
  * @param endowment value that appears to be transferred between accounts,
  *                  if CALLCODE - equal to callValue (but is not really transferred)
  *                  if DELEGATECALL - always zero
  *                  otherwise - equal to value
  * @param doTransfer false for CALLCODE/DELEGATECALL, true otherwise
  * @param startGas initial gas for the execution
  * @param world provides interactions with world state
  * @param initialAddressesToDelete contains initial set of addresses to delete (from lower depth calls)
  * @param evmConfig evm config
  *
  */
case class ProgramContext[W <: WorldStateProxy[W, S], S <: Storage[S]](
  callerAddr: Address,
  originAddr: Address,
  recipientAddr: Option[Address],
  gasPrice: UInt256,
  startGas: BigInt,
  inputData: ByteString,
  value: UInt256,
  endowment: UInt256,
  doTransfer: Boolean,
  blockHeader: BlockHeader,
  callDepth: Int,
  world: W,
  initialAddressesToDelete: Set[Address],
  evmConfig: EvmConfig
) {
  require(startGas >= 0, "start gas should not be less than 0")
}

