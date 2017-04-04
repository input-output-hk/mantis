package io.iohk.ethereum.vmrunner

import akka.util.ByteString
import io.iohk.ethereum.domain.{Account, Address}
import io.iohk.ethereum.vm._
import WorldState.{PC, PR}

object State {

  val creatorAddress = Address(0xabcdef) //scalastyle:off magic.number

  private var currentWorld = {
    val creatorAcc = Account.Empty.updateBalance(UInt256(10) ** UInt256(9))
    val creatorXAcc = XAccount(creatorAcc, "Creator", creatorAddress, Nil)
    WorldState().saveXAccount(creatorAddress, creatorXAcc)
  }

  def world: WorldState = currentWorld


  def createAccount(name: String, balance: BigInt, gas: BigInt, code: ByteString, abis: Seq[ABI]): (Address, PR) = {
    val (newAddress, _) = world.newAddress(creatorAddress)
    val tx = MockVmInput.transaction(creatorAddress, code, balance, gas)
    val bh = MockVmInput.blockHeader

    val context: PC = ProgramContext(tx, bh, world, EvmConfig.HomesteadConfig)
    val intermediateResult: PR = VM.run(context)

    val result: PR = if (intermediateResult.error.isDefined) intermediateResult else {
      // TODO: val depositCost = GasFee.G_codedeposit * intermediateResult.returnData.size
      val depositCost = 0 // GasFee.G_codedeposit * intermediateResult.returnData.size
      intermediateResult.copy(
        gasRemaining = intermediateResult.gasRemaining - depositCost,
        error = if (intermediateResult.gasRemaining < depositCost) Some(OutOfGas) else None
      )
    }

    if (result.error.isEmpty) {
      val account = result.world.getGuaranteedAccount(newAddress)
      val xAccount = XAccount(account, name, newAddress, abis)

      val world1 = result.world
        .saveXAccount(newAddress, xAccount)
        .saveCode(newAddress, result.returnData)

      val world2 = result.addressesToDelete.foldLeft(world1)(_ deleteAccount _)

      currentWorld = world2
    }

    (newAddress, result)
  }

  def runTransaction(xAccount: XAccount, callData: ByteString, gas: BigInt, value: BigInt): PR = {
    val tx = MockVmInput.transaction(creatorAddress, callData, value, gas, receivingAddress = xAccount.address)
    val bh = MockVmInput.blockHeader

    val context: PC = ProgramContext(tx, bh, world, EvmConfig.HomesteadConfig)
    val result: PR = VM.run(context)

    if (result.error.isEmpty) {
      val world = result.world
      val world1 = result.addressesToDelete.foldLeft(world)(_ deleteAccount _)
      currentWorld = world1
    }

    result
  }
}
