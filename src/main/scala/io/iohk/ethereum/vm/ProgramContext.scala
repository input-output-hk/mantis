package io.iohk.ethereum.vm

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, BlockHeader, SignedTransaction}


object ProgramContext {
  def apply(stx: SignedTransaction, blockHeader: BlockHeader, accounts: AccountRetriever): ProgramContext = {
    import stx.tx._
    val maybeAccount = accounts.getAccount(receivingAddress)
    val senderAddress = stx.recoveredSenderAddress.get // FIXME: get, it should be validated but...

    val (program, storages) = maybeAccount match {
      case Some(account) =>
        accounts.getProgram(account) -> Map(account.storageRoot -> accounts.getStorage(account))

      case None =>
        Program(ByteString.empty) -> Map(ByteString.empty -> Storage.Empty)
    }

    val env = ExecEnv(receivingAddress, senderAddress, gasPrice, payload, senderAddress,
      value, program, blockHeader, callDepth = 0)

    ProgramContext(env, gasLimit, maybeAccount.getOrElse(Account.Empty), accounts, storages)
  }
}
/**
  * Input parameters to a program executed on the EVM. Apart from the code itself
  * it should have all (interfaces to) the data accessible from the EVM.
  *
  * @param env set of constants for the execution
  * @param startGas initial gas for the execution
  * @param account this contract's account
  * @param storages the storages of accounts to be modified throughout the execution of the code.
  *                 If the execution agent is a Transaction this will be a single storage
  *                 Multiple storages if this is a context of internal (recursive) invocation of the VM
  *                 via the CALL opcode.
  *                 The map key is the storage root hash
  * @param accounts used to retrieve an account's code and storage
  */
case class ProgramContext(
  env: ExecEnv,
  startGas: BigInt,
  account: Account,
  accounts: AccountRetriever,
  storages: Map[ByteString, Storage])
