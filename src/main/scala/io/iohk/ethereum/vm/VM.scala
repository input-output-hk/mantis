package io.iohk.ethereum.vm

import akka.util.ByteString

import scala.annotation.tailrec

import io.iohk.ethereum.domain.AccessListItem
import io.iohk.ethereum.domain.Address
import io.iohk.ethereum.domain.UInt256
import io.iohk.ethereum.utils.Logger

class VM[W <: WorldStateProxy[W, S], S <: Storage[S]] extends Logger {

  type PC = ProgramContext[W, S]
  type PR = ProgramResult[W, S]
  type PS = ProgramState[W, S]

  /** Executes a top-level program (transaction)
    * @param context context to be executed
    * @return result of the execution
    */
  def run(context: ProgramContext[W, S]): ProgramResult[W, S] = {
    {
      import context._
      import org.bouncycastle.util.encoders.Hex
      log.trace(
        s"caller:  $callerAddr | recipient: $recipientAddr | gasPrice: $gasPrice | value: $value | inputData: ${Hex
          .toHexString(inputData.toArray)}"
      )
    }

    context.recipientAddr match {
      case Some(recipientAddr) =>
        call(context, recipientAddr)

      case None =>
        create(context)._1
    }
  }

  /** Message call - Θ function in YP
    */
  private[vm] def call(context: PC, ownerAddr: Address): PR =
    if (!isValidCall(context))
      invalidCallResult(context, Set.empty, Set.empty)
    else {
      require(context.recipientAddr.isDefined, "Recipient address must be defined for message call")

      def makeTransfer = context.world.transfer(context.callerAddr, context.recipientAddr.get, context.endowment)
      val world1 = if (context.doTransfer) makeTransfer else context.world
      val context1: PC = context.copy(world = world1)

      if (PrecompiledContracts.isDefinedAt(context1))
        PrecompiledContracts.run(context1)
      else {
        val code = world1.getCode(context.recipientAddr.get)
        val env = ExecEnv(context1, code, ownerAddr)

        val initialState: PS = ProgramState(this, context1, env)
        exec(initialState).toResult
      }
    }

  /** Contract creation - Λ function in YP
    * salt is used to create contract by CREATE2 opcode. See https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1014.md
    */
  private[vm] def create(
      context: PC,
      salt: Option[UInt256] = None
  ): (PR, Address) =
    if (!isValidCall(context))
      (invalidCallResult(context, Set.empty, Set.empty), Address(0))
    else {
      require(context.recipientAddr.isEmpty, "recipient address must be empty for contract creation")
      require(context.doTransfer, "contract creation will alwyas transfer funds")

      val newAddress = salt
        .map(s => context.world.create2Address(context.callerAddr, s, context.inputData))
        .getOrElse(context.world.createAddress(context.callerAddr))

      // EIP-684
      // Need to check for conflicts before initialising account (initialisation set account codehash and storage root
      // to empty values.
      val conflict = context.world.nonEmptyCodeOrNonceAccount(newAddress)

      /** Specification of https://eips.ethereum.org/EIPS/eip-1283 states, that `originalValue` should be taken from
        *  world which is left after `a reversion happens on the current transaction`, so in current scope `context.originalWorld`.
        *
        *  But ets test expects that it should be taken from world after the new account initialisation, which clears
        *  account storage.
        *  As it seems other implementations encountered similar problems with this ambiguity:
        *  ambiguity:
        *  https://gist.github.com/holiman/0154f00d5fcec5f89e85894cbb46fcb2 - explanation of geth and parity treating this
        *  situation differently.
        *  https://github.com/mana-ethereum/mana/pull/579 - elixir eth client dealing with same problem.
        */
      val originInitialisedAccount = context.originalWorld.initialiseAccount(newAddress)

      val world1: W =
        context.world.initialiseAccount(newAddress).transfer(context.callerAddr, newAddress, context.endowment)

      val code = if (conflict) ByteString(INVALID.code) else context.inputData

      val env = ExecEnv(context, code, newAddress).copy(inputData = ByteString.empty)

      val initialState: PS =
        ProgramState(this, context.copy(world = world1, originalWorld = originInitialisedAccount): PC, env)
          .addAccessedAddress(newAddress)

      val execResult = exec(initialState).toResult

      val newContractResult = saveNewContract(context, newAddress, execResult, env.evmConfig)
      (newContractResult, newAddress)
    }

  @tailrec
  final private[vm] def exec(state: ProgramState[W, S]): ProgramState[W, S] = {
    val byte = state.program.getByte(state.pc)
    state.config.byteToOpCode.get(byte) match {
      case Some(opCode) =>
        val newState = opCode.execute(state)
        import newState._
        log.trace(
          s"$opCode | pc: $pc | depth: ${env.callDepth} | gasUsed: ${state.gas - gas} | gas: $gas | stack: $stack"
        )
        if (newState.halted)
          newState
        else
          exec(newState)

      case None =>
        state.withError(InvalidOpCode(byte)).halt
    }
  }

  protected def isValidCall(context: PC): Boolean =
    context.endowment <= context.world.getBalance(context.callerAddr) &&
      context.callDepth <= EvmConfig.MaxCallDepth

  private def invalidCallResult(
      context: PC,
      accessedAddresses: Set[Address],
      accessedStorageKeys: Set[(Address, BigInt)]
  ): PR =
    ProgramResult(
      ByteString.empty,
      context.startGas,
      context.world,
      Set(),
      Nil,
      Nil,
      0,
      Some(InvalidCall),
      accessedAddresses,
      accessedStorageKeys
    )

  private def exceedsMaxContractSize(context: PC, config: EvmConfig, contractCode: ByteString): Boolean = {
    lazy val maxCodeSizeExceeded = config.maxCodeSize.exists(codeSizeLimit => contractCode.size > codeSizeLimit)
    val currentBlock = context.blockHeader.number
    // Max code size was enabled on eip161 block number on eth network, and on atlantis block number on etc
    (currentBlock >= config.blockchainConfig.eip161BlockNumber || currentBlock >= config.blockchainConfig.atlantisBlockNumber) &&
    maxCodeSizeExceeded
  }

  private def saveNewContract(context: PC, address: Address, result: PR, config: EvmConfig): PR =
    if (result.error.isDefined) {
      if (result.error.contains(RevertOccurs)) result else result.copy(gasRemaining = 0)
    } else {
      val contractCode = result.returnData
      val codeDepositCost = config.calcCodeDepositCost(contractCode)

      val maxCodeSizeExceeded = exceedsMaxContractSize(context, config, contractCode)
      val codeStoreOutOfGas = result.gasRemaining < codeDepositCost

      if (maxCodeSizeExceeded || (codeStoreOutOfGas && config.exceptionalFailedCodeDeposit)) {
        // Code size too big or code storage causes out-of-gas with exceptionalFailedCodeDeposit enabled
        result.copy(error = Some(OutOfGas), gasRemaining = 0)
      } else if (codeStoreOutOfGas && !config.exceptionalFailedCodeDeposit) {
        // Code storage causes out-of-gas with exceptionalFailedCodeDeposit disabled
        result
      } else {
        // Code storage succeeded
        result.copy(
          gasRemaining = result.gasRemaining - codeDepositCost,
          world = result.world.saveCode(address, result.returnData)
        )
      }
    }
}
