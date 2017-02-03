package io.iohk.ethereum.vm

import akka.util.ByteString

/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param program the bytecode to be executed
  * @param callData data (payload) of the initiating transaction (may be replaced with [[io.iohk.ethereum.network.p2p.messages.CommonMessages.Transaction]]
  * @param callValue value of the initiating transaction (may be replaced with [[io.iohk.ethereum.network.p2p.messages.CommonMessages.Transaction]]
  * @param storage representation of the storage accociated with the contract to be executed
  */
case class ProgramContext(program: Program, gas: BigInt, callData: ByteString, callValue: ByteString, storage: Storage) {
  require(callValue.length <= 32, "Invalid callValue")
}
