package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Address, Transaction}

/**
 * Represenation of the result of execution of a contract
 *
 * @param returnData bytes returned by the executed contract (set by [[RETURN]] opcode)
 * @param storage represents changes to the storage of this account
 * @param transactions list of transactions created during run of the program
 * @param addressesToDelete list of addresses of accounts scheduled to be deleted
 * @param error defined when the program terminated abnormally
 */
case class ProgramResult(returnData: ByteString,
                         storage: Storage,
                         transactions: Seq[Transaction] = Seq(),
                         addressesToDelete: Seq[Address] = Seq(),
                         error: Option[ProgramError])
