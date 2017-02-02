package io.iohk.ethereum.vm

import akka.util.ByteString

/**
  * Represenation of the result of execution of a contract
  *
  * @param returnData bytes returned by the executed contract (set by [[RETURN]] opcode)
  * @param storage represents changes to the storage of this account
  * @param error defined when the program terminated abnormally
  */
case class ProgramResult(returnData: ByteString, storage: Storage, error: Option[ProgramError])
