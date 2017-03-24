package io.iohk.ethereum.vm

import akka.util.ByteString

/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param env set of constants for the execution
  * @param startGas initial gas for the execution
  * @param storage representation of the storage accociated with the contract to be executed
  */
case class ProgramContext(env: ExecEnv, startGas: BigInt, storage: Storage)
