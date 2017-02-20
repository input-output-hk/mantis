package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}

/**
 * Represenation of the result of execution of a contract
 *
 * @param returnData bytes returned by the executed contract (set by [[RETURN]] opcode)
 * @param gasRemaining amount of gas remaining after execution
 * @param gasUsed amount of gas used in execution
 * @param storages represents changes to the storages of this account and called accounts (the map key is storage root hash)
 * @param internalTransfers list of value transfers created during run of the program
 * @param addressesToDelete list of addresses of accounts scheduled to be deleted
 * @param error defined when the program terminated abnormally
 */
case class ProgramResult(returnData: ByteString,
                         gasRemaining: BigInt,
                         gasUsed: BigInt,
                         storages: Map[ByteString, Storage],
                         internalTransfers: Seq[Transfer] = Nil,
                         addressesToDelete: Seq[Address] = Seq(),
                         error: Option[ProgramError])
