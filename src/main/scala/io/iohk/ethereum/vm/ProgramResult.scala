package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.Address

/**
 * Represenation of the result of execution of a contract
 *
 * @param returnData bytes returned by the executed contract (set by [[RETURN]] opcode)
 * @param storage represents changes to the storage of this account
 * @param internalTransfers list of value transfers created during run of the program
 * @param addressesToDelete list of addresses of accounts scheduled to be deleted
 * @param error defined when the program terminated abnormally
 */
case class ProgramResult(returnData: ByteString,
                         storage: Storage,
                         internalTransfers: Seq[Transfer] = Nil,
                         addressesToDelete: Seq[Address] = Seq(),
                         error: Option[ProgramError])
